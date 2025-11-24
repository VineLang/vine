use std::collections::{BTreeMap, BTreeSet, HashSet};

use vine_util::idx::IntSet;

use crate::{
  components::synthesizer::SyntheticImpl,
  structures::{
    ast::{Ident, Span},
    chart::{
      Binding, Chart, Def, DefId, DefImplKind, DefTraitKind, DefValueKind, FnId, GenericsId,
      ImplId, MemberKind,
    },
    diag::{Diag, Diags, ErrorGuaranteed},
    signatures::{ImportState, Signatures},
    tir::TirImpl,
    types::{ImplType, Inverted, Type, TypeCtx, TypeKind, Types},
  },
};

pub struct Finder<'a> {
  chart: &'a Chart,
  sigs: &'a Signatures,
  diags: &'a mut Diags,
  source: DefId,
  generics: GenericsId,
  span: Span,
  steps: u32,
}

#[derive(Debug, Default)]
pub struct FlexImpls {
  pub inv: Inverted,
  pub fork: Option<TirImpl>,
  pub drop: Option<TirImpl>,
  pub self_dual: bool,
}

struct CandidateSearch<F> {
  mods: IntSet<DefId>,
  defs: IntSet<DefId>,
  consider_candidate: F,
}

const STEPS_LIMIT: u32 = 1_000;

#[derive(Debug, Clone, Copy)]
enum Error {
  Timeout,
  Guaranteed(ErrorGuaranteed),
}

impl From<ErrorGuaranteed> for Error {
  fn from(err: ErrorGuaranteed) -> Self {
    Error::Guaranteed(err)
  }
}

impl<'a> Finder<'a> {
  pub fn new(
    chart: &'a Chart,
    sigs: &'a Signatures,
    diags: &'a mut Diags,
    source: DefId,
    generics: GenericsId,
    span: Span,
  ) -> Self {
    Finder { chart, sigs, diags, source, generics, span, steps: 0 }
  }

  pub fn find_method(
    &mut self,
    types: &Types,
    receiver: Type,
    name: Ident,
  ) -> Result<Vec<(Span, FnId, TypeCtx<Vec<Type>>)>, ErrorGuaranteed> {
    let mut found = Vec::new();

    for (candidate, span) in self.find_method_candidates(types, receiver, name)? {
      let mut types = types.clone();
      let generics = self.chart.fn_generics(candidate);
      let type_params = types.new_vars(self.span, self.sigs.type_params[generics].params.len());
      let candidate_receiver =
        types.import_with(self.sigs.fn_sig(candidate), Some(&type_params), |t, sig| {
          sig.param_tys.first().map(|&ty| t.transfer(&ty))
        });
      if let Some(candidate_receiver) = candidate_receiver {
        let candidate_receiver = match types.kind(candidate_receiver) {
          Some((Inverted(false), TypeKind::Ref(t))) => *t,
          _ => candidate_receiver,
        };
        if types.unify(receiver, candidate_receiver).is_success() {
          found.push((span, candidate, TypeCtx { types, inner: type_params }));
        }
      }
    }

    Ok(found)
  }

  fn find_method_candidates(
    &mut self,
    types: &Types,
    receiver: Type,
    name: Ident,
  ) -> Result<BTreeMap<FnId, Span>, ErrorGuaranteed> {
    let mut candidates = BTreeMap::new();
    let search = &mut CandidateSearch {
      mods: HashSet::default(),
      defs: HashSet::default(),
      consider_candidate: |self_: &Self, def: &Def| {
        if def.name == name
          && let Some(Binding { span, vis, kind: DefValueKind::Fn(fn_kind), .. }) = def.value_kind
          && self_.chart.visible(vis, self_.source)
          && self_.chart.fn_is_method(fn_kind)
        {
          candidates.insert(fn_kind, span);
        }
      },
    };

    self.find_general_candidates(search);

    if let Some(def_id) = types.get_mod(self.chart, receiver)? {
      self.consider_mod(search, def_id);
    }

    Ok(candidates)
  }

  pub fn find_flex(&mut self, types: &mut Types, ty: Type) -> Result<FlexImpls, ErrorGuaranteed> {
    self._find_flex(types, ty).map_err(|diag| self.diags.report(diag))
  }

  fn _find_flex(&mut self, types: &mut Types, ty: Type) -> Result<FlexImpls, Diag> {
    let span = self.span;
    let TypeCtx { types: sub_types, inner: sub_ty } = types.export(|t| t.transfer(&ty));

    let Some(fork) = self.chart.builtins.fork else {
      Err(Diag::MissingBuiltin { span, builtin: "Fork" })?
    };
    let Some(drop) = self.chart.builtins.drop else {
      Err(Diag::MissingBuiltin { span, builtin: "Drop" })?
    };

    let results =
      [(fork, sub_ty), (drop, sub_ty), (fork, sub_ty.inverse()), (drop, sub_ty.inverse())].map(
        |(trait_, ty)| {
          let query = ImplType::Trait(trait_, vec![ty]);
          let results = self._find_impl(&sub_types, &query, false).unwrap_or_default();
          (query, results)
        },
      );

    if self.steps > STEPS_LIMIT {
      Err(Diag::FlexSearchLimit { span: self.span, ty: types.show(self.chart, ty) })?
    }

    let mut error = Ok(());
    let [pos_fork, pos_drop, neg_fork, neg_drop] = results.map(|(query, mut results)| {
      if results.len() > 1 {
        let diag = Diag::AmbiguousImpl { span, ty: sub_types.show_impl_type(self.chart, &query) };
        error = Err(self.diags.report(diag));
        None
      } else {
        results.pop()
      }
    });
    error?;

    let pos_flex = pos_fork.is_some() || pos_drop.is_some();
    let neg_flex = neg_fork.is_some() || neg_drop.is_some();
    let self_dual = types.self_dual(ty);

    if pos_flex && neg_flex && !self_dual {
      Err(Diag::BiFlexible { span, ty: types.show(self.chart, ty) })?
    }

    let inv = Inverted(!pos_flex && neg_flex);
    let [fork, drop] = if inv.0 { [neg_fork, neg_drop] } else { [pos_fork, pos_drop] };

    let [(fork_ty, fork), (drop_ty, drop)] = [fork, drop].map(|result| {
      (
        result.as_ref().map(|result| types.import_with(result, None, |t, _| t.transfer(&sub_ty))),
        result.map(|result| result.inner),
      )
    });

    if let (Some(fork_ty), Some(drop_ty)) = (fork_ty, drop_ty)
      && types.unify(fork_ty, drop_ty).is_failure()
    {
      Err(Diag::IncompatibleForkDropInference {
        span,
        ty: types.show(self.chart, ty.invert_if(inv)),
        fork_ty: types.show(self.chart, fork_ty.invert_if(inv)),
        drop_ty: types.show(self.chart, drop_ty.invert_if(inv)),
      })?
    }

    if let Some(new_ty) = fork_ty.or(drop_ty) {
      let unify_result = types.unify(ty, new_ty);
      assert!(!unify_result.is_failure());
    }

    Ok(FlexImpls { inv, fork, drop, self_dual })
  }

  pub fn find_impl(&mut self, types: &mut Types, query: &ImplType, basic: bool) -> TirImpl {
    self
      .try_find_impl(types, query, basic)
      .unwrap_or_else(|err| TirImpl::Error(self.diags.report(err)))
  }

  pub fn try_find_impl(
    &mut self,
    types: &mut Types,
    query: &ImplType,
    basic: bool,
  ) -> Result<TirImpl, Diag> {
    let span = self.span;
    let TypeCtx { types: sub_types, inner: sub_query } = types.export(|t| t.transfer(query));

    let show_ty = || types.show_impl_type(self.chart, query);
    let mut results = match self._find_impl(&sub_types, &sub_query, basic) {
      Ok(results) => results,
      Err(Error::Timeout) => Err(Diag::SearchLimit { span, ty: show_ty() })?,
      Err(Error::Guaranteed(err)) => Err(err)?,
    };

    if results.is_empty() {
      return Err(Diag::CannotFindImpl { span, ty: show_ty() });
    }

    if results.len() > 1 {
      return Err(Diag::AmbiguousImpl { span, ty: show_ty() });
    }

    let result = results.pop().unwrap();

    let result_ty = types.import_with(&result, None, |t, _| t.transfer(&sub_query));
    let unify_result = types.unify_impl_type(query, &result_ty);
    assert!(!unify_result.is_failure());

    Ok(result.inner)
  }

  fn _find_impl(
    &mut self,
    types: &Types,
    query: &ImplType,
    basic: bool,
  ) -> Result<Vec<TypeCtx<TirImpl>>, Error> {
    self.step()?;

    let mut found = Vec::new();

    if !basic {
      self.find_auto_impls(types, query, &mut found)?;

      let impl_params = &self.sigs.impl_params[self.generics];
      for (i, ty) in impl_params.types.inner.iter().enumerate() {
        if query.approx_eq(ty) {
          let mut types = types.clone();
          let ty = types.import_with(&impl_params.types, None, |t, _| t.transfer(ty));
          if types.unify_impl_type(&ty, query).is_success() {
            found.push(TypeCtx { types, inner: TirImpl::Param(i) });
          }
        }
      }
    }

    for candidate in self.find_impl_candidates(types, query, basic)? {
      let mut types = types.clone();
      let generics = self.chart.impls[candidate].generics;
      let type_params = (0..self.sigs.type_params[generics].params.len())
        .map(|_| types.new_var(self.span))
        .collect::<Vec<_>>();
      let ty = types.import(&self.sigs.impls[candidate], Some(&type_params)).ty;
      if types.unify_impl_type(&ty, query).is_success() {
        for result in self.find_impl_params(types, generics, type_params)? {
          found.push(TypeCtx { types: result.types, inner: TirImpl::Def(candidate, result.inner) });
        }
      }
    }

    Ok(found)
  }

  fn find_impl_params(
    &mut self,
    mut types: Types,
    generics: GenericsId,
    type_params: Vec<Type>,
  ) -> Result<impl Iterator<Item = TypeCtx<Vec<TirImpl>>>, Error> {
    let queries = types.import(&self.sigs.impl_params[generics].types, Some(&type_params));
    let results = self.find_subimpls(types, &queries)?;
    Ok(results.into_iter().map(|mut result| {
      result.inner.reverse();
      result
    }))
  }

  fn find_subimpls(
    &mut self,
    types: Types,
    queries: &[ImplType],
  ) -> Result<Vec<TypeCtx<Vec<TirImpl>>>, Error> {
    let [query, rest_queries @ ..] = queries else {
      return Ok(vec![(TypeCtx { types, inner: vec![] })]);
    };

    let mut found = Vec::new();

    let impls = self._find_impl(&types, query, false)?;
    for impl_result in impls {
      let rest = self.find_subimpls(impl_result.types, rest_queries)?;
      found.extend(rest.into_iter().map(|mut subimpls_result| {
        subimpls_result.inner.push(impl_result.inner.clone());
        subimpls_result
      }));
    }

    Ok(found)
  }

  fn find_impl_candidates(
    &mut self,
    types: &Types,
    query: &ImplType,
    basic: bool,
  ) -> Result<BTreeSet<ImplId>, ErrorGuaranteed> {
    let mut candidates = BTreeSet::new();
    let search = &mut CandidateSearch {
      mods: HashSet::default(),
      defs: HashSet::default(),
      consider_candidate: |self_: &Self, def: &Def| {
        if let Some(Binding { vis, kind: DefImplKind::Impl(impl_id), .. }) = def.impl_kind
          && self_.chart.visible(vis, self_.source)
        {
          let impl_ = &self_.chart.impls[impl_id];
          if !impl_.manual && impl_.basic == basic {
            let impl_sig = &self_.sigs.impls[impl_id];
            if impl_sig.inner.ty.approx_eq(query) {
              candidates.insert(impl_id);
            }
          }
        }
      },
    };

    self.find_general_candidates(search);

    if let ImplType::Trait(trait_id, params) = query {
      self.consider_mod(search, self.chart.traits[*trait_id].def);
      for &param in params {
        if let Some(mod_id) = types.get_mod(self.chart, param)? {
          self.consider_mod(search, mod_id);
        }
      }
    }

    Ok(candidates)
  }

  fn find_general_candidates(&mut self, search: &mut CandidateSearch<impl FnMut(&Self, &Def)>) {
    for &ancestor in &self.chart.defs[self.source].ancestors {
      self.consider_mod(search, ancestor);
    }
    if let Some(prelude) = self.chart.builtins.prelude {
      self.consider_mod(search, prelude);
    }
  }

  fn consider_mod<F: FnMut(&Self, &Def)>(
    &mut self,
    search: &mut CandidateSearch<F>,
    mod_id: DefId,
  ) {
    if !search.mods.insert(mod_id) {
      return;
    }

    self.consider_def(search, mod_id);

    for member in &self.chart.defs[mod_id].named_members {
      self.consider_member(search, member);
    }
  }

  fn consider_member<F: FnMut(&Self, &Def)>(
    &mut self,
    search: &mut CandidateSearch<F>,
    member: &Binding<MemberKind>,
  ) {
    if !self.chart.visible(member.vis, self.source) {
      return;
    }

    let def_id = match member.kind {
      MemberKind::Child(def_id) => Some(def_id),
      MemberKind::Import(import_id) => match self.sigs.imports[import_id] {
        ImportState::Resolved(Ok(def_id)) => Some(def_id),
        _ => None,
      },
    };

    if let Some(def_id) = def_id {
      self.consider_def(search, def_id);
    }
  }

  fn consider_def<F: FnMut(&Self, &Def)>(
    &mut self,
    search: &mut CandidateSearch<F>,
    def_id: DefId,
  ) {
    if !search.defs.insert(def_id) {
      return;
    }

    let def = &self.chart.defs[def_id];

    for member in &def.implicit_members {
      self.consider_member(search, member);
    }

    (search.consider_candidate)(self, def);

    if let Some(Binding { kind: DefTraitKind::Trait(trait_id), vis, .. }) = def.trait_kind
      && self.chart.visible(vis, self.source)
    {
      let trait_def = &self.chart.traits[trait_id];
      self.consider_mod(search, trait_def.def);
    }

    if let Some(Binding { kind: DefImplKind::Impl(impl_id), vis, .. }) = def.impl_kind
      && self.chart.visible(vis, self.source)
    {
      let impl_sig = &self.sigs.impls[impl_id];
      if let ImplType::Trait(trait_id, _) = impl_sig.inner.ty {
        let trait_def = &self.chart.traits[trait_id];
        self.consider_mod(search, trait_def.def);
      }
    }
  }

  fn step(&mut self) -> Result<(), Error> {
    self.steps += 1;
    if self.steps > STEPS_LIMIT { Err(Error::Timeout) } else { Ok(()) }
  }

  fn find_auto_impls(
    &mut self,
    types: &Types,
    query: &ImplType,
    found: &mut Vec<TypeCtx<TirImpl>>,
  ) -> Result<(), Error> {
    match query {
      ImplType::Trait(trait_id, type_params) => {
        if Some(*trait_id) == self.chart.builtins.fn_ {
          let [receiver, params, ret] = **type_params else { unreachable!() };
          match types.kind(receiver) {
            Some((Inverted(false), TypeKind::Fn(fn_id))) => {
              let mut types = types.clone();
              let generics = self.chart.fn_generics(*fn_id);
              let type_params =
                types.new_vars(self.span, self.sigs.type_params[generics].params.len());
              let sig = types.import(self.sigs.fn_sig(*fn_id), Some(&type_params));
              let param_count = sig.param_tys.len();
              let sig_params = types.new(TypeKind::Tuple(sig.param_tys));
              if types.unify(sig_params, params).and(types.unify(sig.ret_ty, ret)).is_success() {
                for result in self.find_impl_params(types, generics, type_params)? {
                  found.push(TypeCtx {
                    types: result.types,
                    inner: TirImpl::Fn(*fn_id, result.inner, param_count),
                  });
                }
              }
            }
            Some((Inverted(false), TypeKind::Closure(closure_id, _, closure_sig))) => {
              let param_count = closure_sig.param_tys.len();
              let mut types = types.clone();
              let closure_params = types.new(TypeKind::Tuple(closure_sig.param_tys.clone()));
              if types
                .unify(closure_params, params)
                .and(types.unify(closure_sig.ret_ty, ret))
                .is_success()
              {
                found.push(TypeCtx { types, inner: TirImpl::Closure(*closure_id, param_count) });
              }
            }
            _ => {}
          }
        }
        if Some(*trait_id) == self.chart.builtins.fork {
          match types.kind(type_params[0]) {
            Some((Inverted(false), TypeKind::Fn(_))) => {
              if let Some(dup) = self.chart.builtins.duplicate {
                found.push(TypeCtx { types: types.clone(), inner: TirImpl::Def(dup, vec![]) });
              }
            }
            Some((Inverted(false), TypeKind::Closure(id, flex, ..))) if flex.fork() => {
              found.push(TypeCtx { types: types.clone(), inner: TirImpl::ForkClosure(*id) });
            }
            _ => {}
          }
        }
        if Some(*trait_id) == self.chart.builtins.drop {
          match types.kind(type_params[0]) {
            Some((Inverted(false), TypeKind::Fn(_))) => {
              if let Some(erase) = self.chart.builtins.erase {
                found.push(TypeCtx { types: types.clone(), inner: TirImpl::Def(erase, vec![]) });
              }
            }
            Some((Inverted(false), TypeKind::Closure(id, flex, ..))) if flex.drop() => {
              found.push(TypeCtx { types: types.clone(), inner: TirImpl::DropClosure(*id) });
            }
            _ => {}
          }
        }
        if Some(*trait_id) == self.chart.builtins.tuple {
          if let Some((inv, TypeKind::Tuple(elements))) = types.kind(type_params[0]) {
            if let [init, ref rest @ ..] = **elements {
              let init = init.invert_if(inv);
              let rest = rest.iter().map(|t| t.invert_if(inv)).collect();
              let mut types = types.clone();
              let rest = types.new(TypeKind::Tuple(rest));
              if types
                .unify(init, type_params[1])
                .and(types.unify(rest, type_params[2]))
                .is_success()
              {
                let impl_ = TirImpl::Synthetic(SyntheticImpl::Tuple(elements.len()));
                found.push(TypeCtx { types, inner: impl_ });
              }
            }
          } else if let Some((inv, TypeKind::Tuple(rest))) = types.kind(type_params[2]) {
            let len = 1 + rest.len();
            let init = type_params[1];
            let rest = rest.iter().map(|t| t.invert_if(inv));
            let mut types = types.clone();
            let tuple = types.new(TypeKind::Tuple([init].into_iter().chain(rest).collect()));
            if types.unify(tuple, type_params[0]).is_success() {
              let impl_ = TirImpl::Synthetic(SyntheticImpl::Tuple(len));
              found.push(TypeCtx { types, inner: impl_ });
            }
          }
        }
        if Some(*trait_id) == self.chart.builtins.object
          && let Some((inv, TypeKind::Object(entries))) = types.kind(type_params[0])
        {
          let mut iter = entries.iter();
          if let Some((key, &init)) = iter.next() {
            let init = init.invert_if(inv);
            let rest = iter.map(|(k, &t)| (k.clone(), t.invert_if(inv))).collect();
            let mut types = types.clone();
            let rest = types.new(TypeKind::Object(rest));
            if types.unify(init, type_params[1]).and(types.unify(rest, type_params[2])).is_success()
            {
              let impl_ = TirImpl::Synthetic(SyntheticImpl::Object(key.clone(), entries.len()));
              found.push(TypeCtx { types, inner: impl_ });
            }
          }
        }
        if Some(*trait_id) == self.chart.builtins.struct_
          && let Some((Inverted(false), TypeKind::Struct(struct_id, struct_params))) =
            types.kind(type_params[0])
        {
          let struct_ = &self.chart.structs[*struct_id];
          if self.chart.visible(struct_.data_vis, self.source) {
            let mut types = types.clone();
            let content = types.import(&self.sigs.structs[*struct_id], Some(struct_params)).data;
            if types.unify(content, type_params[1]).is_success() {
              let impl_ = TirImpl::Synthetic(SyntheticImpl::Struct(*struct_id));
              found.push(TypeCtx { types, inner: impl_ });
            }
          }
        }
        if Some(*trait_id) == self.chart.builtins.enum_
          && let Some(variant_enum) = self.chart.builtins.variant
          && let Some((Inverted(false), TypeKind::Enum(enum_id, enum_params))) =
            types.kind(type_params[0])
        {
          let mut types = types.clone();
          let sig = types.import(&self.sigs.enums[*enum_id], Some(enum_params));
          let nil = types.nil();
          let variants =
            sig.variant_data.values().rfold(types.new(TypeKind::Never), |rest, init| {
              types.new(TypeKind::Enum(variant_enum, vec![init.unwrap_or(nil), rest]))
            });
          if types.unify(variants, type_params[1]).is_success() {
            found.push(TypeCtx { types, inner: TirImpl::Synthetic(SyntheticImpl::Enum(*enum_id)) });
          }
        }
      }
      ImplType::Error(_) => {}
    }
    Ok(())
  }
}
