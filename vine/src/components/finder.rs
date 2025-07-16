use std::collections::{BTreeSet, HashSet};

use vine_util::idx::IntSet;

use crate::structures::{
  ast::{Ident, Span},
  chart::{
    Chart, Def, DefId, DefImplKind, DefTraitKind, DefValueKind, FnId, GenericsId, ImplId,
    MemberKind, WithVis,
  },
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  signatures::{ImportState, Signatures},
  tir::TirImpl,
  types::{ImplType, Inverted, Type, TypeCtx, TypeKind, Types},
};

pub struct Finder<'core, 'a> {
  core: &'core Core<'core>,
  chart: &'a Chart<'core>,
  sigs: &'a Signatures<'core>,
  source: DefId,
  generics: GenericsId,
  span: Span,
  steps: u32,
}

#[derive(Debug, Default)]
pub struct FlexImpls<'core> {
  pub inv: Inverted,
  pub fork: Option<TirImpl<'core>>,
  pub drop: Option<TirImpl<'core>>,
  pub is_nil: bool,
}

struct CandidateSearch<F> {
  modules: IntSet<DefId>,
  consider_candidate: F,
}

const STEPS_LIMIT: u32 = 1_000;

#[derive(Debug, Default, Clone, Copy)]
struct Timeout;

impl<'core, 'a> Finder<'core, 'a> {
  pub fn new(
    core: &'core Core<'core>,
    chart: &'a Chart<'core>,
    sigs: &'a Signatures<'core>,
    source: DefId,
    generics: GenericsId,
    span: Span,
  ) -> Self {
    Finder { core, chart, sigs, source, generics, span, steps: 0 }
  }

  pub fn find_method(
    &mut self,
    types: &Types<'core>,
    receiver: Type,
    name: Ident,
  ) -> Vec<(FnId, TypeCtx<'core, Vec<Type>>)> {
    let mut found = Vec::new();

    for candidate in self.find_method_candidates(types, receiver, name) {
      let mut types = types.clone();
      let generics = self.chart.fn_generics(candidate);
      let type_params = types.new_vars(self.span, self.sigs.type_params[generics].params.len());
      let candidate_receiver =
        types.import_with(self.sigs.fn_sig(candidate), Some(&type_params), |t, sig| {
          sig.params.first().map(|&ty| t.transfer(&ty))
        });
      if let Some(candidate_receiver) = candidate_receiver {
        let candidate_receiver = match types.kind(candidate_receiver) {
          Some((Inverted(false), TypeKind::Ref(t))) => *t,
          _ => candidate_receiver,
        };
        if types.unify(receiver, candidate_receiver).is_success() {
          found.push((candidate, TypeCtx { types, inner: type_params }));
        }
      }
    }

    found
  }

  fn find_method_candidates(
    &mut self,
    types: &Types<'core>,
    receiver: Type,
    name: Ident,
  ) -> BTreeSet<FnId> {
    let mut candidates = BTreeSet::new();
    let search = &mut CandidateSearch {
      modules: HashSet::default(),
      consider_candidate: |self_: &Self, def: &Def| {
        if def.name == name {
          if let Some(WithVis { kind: DefValueKind::Fn(fn_kind), vis }) = def.value_kind {
            if self_.chart.visible(vis, self_.source) && self_.chart.fn_is_method(fn_kind) {
              candidates.insert(fn_kind);
            }
          }
        }
      },
    };

    self.find_general_candidates(search);

    if let Some(def_id) = types.get_mod(self.chart, receiver) {
      self.find_candidates_within(def_id, search);
    }

    candidates
  }

  pub fn find_flex(
    &mut self,
    types: &mut Types<'core>,
    ty: Type,
  ) -> Result<FlexImpls<'core>, ErrorGuaranteed> {
    self._find_flex(types, ty).map_err(|diag| self.core.report(diag))
  }

  fn _find_flex(
    &mut self,
    types: &mut Types<'core>,
    ty: Type,
  ) -> Result<FlexImpls<'core>, Diag<'core>> {
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
          let results = self._find_impl(&sub_types, &query).unwrap_or_default();
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
        error = Err(self.core.report(diag));
        None
      } else {
        results.pop()
      }
    });
    error?;

    let pos_flex = pos_fork.is_some() || pos_drop.is_some();
    let neg_flex = neg_fork.is_some() || neg_drop.is_some();
    let is_nil = types.is_nil(ty);

    if pos_flex && neg_flex && !is_nil {
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

    if let (Some(fork_ty), Some(drop_ty)) = (fork_ty, drop_ty) {
      if types.unify(fork_ty, drop_ty).is_failure() {
        Err(Diag::IncompatibleForkDropInference {
          span,
          ty: types.show(self.chart, ty.invert_if(inv)),
          fork_ty: types.show(self.chart, fork_ty.invert_if(inv)),
          drop_ty: types.show(self.chart, drop_ty.invert_if(inv)),
        })?
      }
    }

    if let Some(new_ty) = fork_ty.or(drop_ty) {
      let unify_result = types.unify(ty, new_ty);
      assert!(!unify_result.is_failure());
    }

    Ok(FlexImpls { inv, fork, drop, is_nil })
  }

  pub fn find_impl(&mut self, types: &mut Types<'core>, query: &ImplType) -> TirImpl<'core> {
    let span = self.span;
    let TypeCtx { types: sub_types, inner: sub_query } = types.export(|t| t.transfer(query));

    let show_ty = || types.show_impl_type(self.chart, query);
    let Ok(mut results) = self._find_impl(&sub_types, &sub_query) else {
      return TirImpl::Error(self.core.report(Diag::SearchLimit { span, ty: show_ty() }));
    };

    if results.is_empty() {
      return TirImpl::Error(self.core.report(Diag::CannotFindImpl { span, ty: show_ty() }));
    }

    if results.len() > 1 {
      return TirImpl::Error(self.core.report(Diag::AmbiguousImpl { span, ty: show_ty() }));
    }

    let result = results.pop().unwrap();

    let result_ty = types.import_with(&result, None, |t, _| t.transfer(&sub_query));
    let unify_result = types.unify_impl_type(query, &result_ty);
    assert!(!unify_result.is_failure());

    result.inner
  }

  fn _find_impl(
    &mut self,
    types: &Types<'core>,
    query: &ImplType,
  ) -> Result<Vec<TypeCtx<'core, TirImpl<'core>>>, Timeout> {
    self.step()?;

    let mut found = Vec::new();

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

    for candidate in self.find_impl_candidates(types, query) {
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
    mut types: Types<'core>,
    generics: GenericsId,
    type_params: Vec<Type>,
  ) -> Result<impl Iterator<Item = TypeCtx<'core, Vec<TirImpl<'core>>>>, Timeout> {
    let queries = types.import(&self.sigs.impl_params[generics].types, Some(&type_params));
    let results = self.find_subimpls(types, &queries)?;
    Ok(results.into_iter().map(|mut result| {
      result.inner.reverse();
      result
    }))
  }

  fn find_subimpls(
    &mut self,
    types: Types<'core>,
    queries: &[ImplType],
  ) -> Result<Vec<TypeCtx<'core, Vec<TirImpl<'core>>>>, Timeout> {
    let [query, rest_queries @ ..] = queries else {
      return Ok(vec![(TypeCtx { types, inner: vec![] })]);
    };

    let mut found = Vec::new();

    let impls = self._find_impl(&types, query)?;
    for impl_result in impls {
      let rest = self.find_subimpls(impl_result.types, rest_queries)?;
      found.extend(rest.into_iter().map(|mut subimpls_result| {
        subimpls_result.inner.push(impl_result.inner.clone());
        subimpls_result
      }));
    }

    Ok(found)
  }

  fn find_impl_candidates(&mut self, types: &Types<'core>, query: &ImplType) -> BTreeSet<ImplId> {
    let mut candidates = BTreeSet::new();
    let search = &mut CandidateSearch {
      modules: HashSet::default(),
      consider_candidate: |self_: &Self, def: &Def| {
        if let Some(WithVis { vis, kind: DefImplKind::Impl(impl_id) }) = def.impl_kind {
          if self_.chart.visible(vis, self_.source) && !self_.chart.impls[impl_id].manual {
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
      self.find_candidates_within(self.chart.traits[*trait_id].def, search);
      for &param in params {
        if let Some(mod_id) = types.get_mod(self.chart, param) {
          self.find_candidates_within(mod_id, search);
        }
      }
    }

    candidates
  }

  fn find_general_candidates(&mut self, search: &mut CandidateSearch<impl FnMut(&Self, &Def<'_>)>) {
    for &ancestor in &self.chart.defs[self.source].ancestors {
      self.find_candidates_within(ancestor, search);
    }
    if let Some(prelude) = self.chart.builtins.prelude {
      self.find_candidates_within(prelude, search);
    }
  }

  fn find_candidates_within<F: FnMut(&Self, &Def)>(
    &mut self,
    mod_id: DefId,
    search: &mut CandidateSearch<F>,
  ) {
    if !search.modules.insert(mod_id) {
      return;
    }

    for member in &self.chart.defs[mod_id].all_members {
      if !self.chart.visible(member.vis, self.source) {
        continue;
      }

      let def_id = match member.kind {
        MemberKind::Child(def_id) => Some(def_id),
        MemberKind::Import(import_id) => match self.sigs.imports[import_id] {
          ImportState::Resolved(Ok(def_id)) => Some(def_id),
          _ => None,
        },
      };
      let Some(def_id) = def_id else { continue };

      let def = &self.chart.defs[def_id];

      (search.consider_candidate)(self, def);

      if let Some(WithVis { kind: DefTraitKind::Trait(trait_id), vis }) = def.trait_kind {
        if self.chart.visible(vis, self.source) {
          let trait_def = &self.chart.traits[trait_id];
          self.find_candidates_within(trait_def.def, search);
        }
      }

      if let Some(WithVis { kind: DefImplKind::Impl(impl_id), vis }) = def.impl_kind {
        if self.chart.visible(vis, self.source) {
          let impl_sig = &self.sigs.impls[impl_id];
          if let ImplType::Trait(trait_id, _) = impl_sig.inner.ty {
            let trait_def = &self.chart.traits[trait_id];
            self.find_candidates_within(trait_def.def, search);
          }
        }
      }
    }
  }

  fn step(&mut self) -> Result<(), Timeout> {
    self.steps += 1;
    if self.steps > STEPS_LIMIT {
      Err(Timeout)
    } else {
      Ok(())
    }
  }

  fn find_auto_impls(
    &mut self,
    types: &Types<'core>,
    query: &ImplType,
    found: &mut Vec<TypeCtx<'core, TirImpl<'core>>>,
  ) -> Result<(), Timeout> {
    match query {
      ImplType::Fn(receiver, params, ret) => match types.kind(*receiver) {
        Some((Inverted(false), TypeKind::Fn(fn_id))) => {
          let mut types = types.clone();
          let generics = self.chart.fn_generics(*fn_id);
          let type_params = types.new_vars(self.span, self.sigs.type_params[generics].params.len());
          let sig = types.import(self.sigs.fn_sig(*fn_id), Some(&type_params));
          if types
            .unify_types(&sig.params, params, Inverted(false))
            .and(types.unify(sig.ret_ty, *ret))
            .is_success()
          {
            for result in self.find_impl_params(types, generics, type_params)? {
              found.push(TypeCtx { types: result.types, inner: TirImpl::Fn(*fn_id, result.inner) });
            }
          }
        }
        Some((Inverted(false), TypeKind::Closure(closure_id, _, closure_params, closure_ret))) => {
          let mut types = types.clone();
          if types
            .unify_types(closure_params, params, Inverted(false))
            .and(types.unify(*closure_ret, *ret))
            .is_success()
          {
            found.push(TypeCtx { types, inner: TirImpl::Closure(*closure_id) });
          }
        }
        _ => {}
      },
      ImplType::Trait(trait_id, type_params) => {
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
                found.push(TypeCtx { types, inner: TirImpl::Tuple(elements.len()) });
              }
            }
          }
        }
        if Some(*trait_id) == self.chart.builtins.object {
          if let Some((inv, TypeKind::Object(entries))) = types.kind(type_params[0]) {
            let mut iter = entries.iter();
            if let Some((&key, &init)) = iter.next() {
              let init = init.invert_if(inv);
              let rest = iter.map(|(&k, &t)| (k, t.invert_if(inv))).collect();
              let mut types = types.clone();
              let rest = types.new(TypeKind::Object(rest));
              if types
                .unify(init, type_params[1])
                .and(types.unify(rest, type_params[2]))
                .is_success()
              {
                found.push(TypeCtx { types, inner: TirImpl::Object(key, entries.len()) });
              }
            }
          }
        }
        if Some(*trait_id) == self.chart.builtins.struct_ {
          if let Some((Inverted(false), TypeKind::Struct(struct_id, struct_params))) =
            types.kind(type_params[0])
          {
            let struct_ = &self.chart.structs[*struct_id];
            if self.chart.visible(struct_.data_vis, self.source) {
              let mut types = types.clone();
              let content = types.import(&self.sigs.structs[*struct_id], Some(struct_params)).data;
              if types.unify(content, type_params[1]).is_success() {
                found.push(TypeCtx { types, inner: TirImpl::Struct(struct_.name) });
              }
            }
          }
        }
      }
      ImplType::Error(_) => {}
    }
    Ok(())
  }
}
