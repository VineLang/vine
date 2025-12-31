use std::{
  collections::{BTreeSet, HashMap},
  hash::Hash,
};

use crate::{
  components::finder::candidates::{CandidateSets, VisSet},
  structures::{
    ast::{Ident, Span},
    chart::{Chart, DefId, FnId, GenericsId, ImplId},
    diag::{Diag, Diags, ErrorGuaranteed},
    signatures::Signatures,
    tir::TirImpl,
    types::{ImplType, Inverted, Type, TypeCtx, TypeKind, Types},
  },
};

pub(crate) mod candidates;

pub struct Finder<'a> {
  pub(crate) chart: &'a Chart,
  pub(crate) sigs: &'a Signatures,
  pub(crate) diags: &'a mut Diags,
  pub(crate) cache: &'a mut FinderCache,
  pub(crate) source: DefId,
  pub(crate) generics: GenericsId,
  pub(crate) span: Span,
  pub(crate) steps: u32,
}

#[derive(Default, Debug, Clone)]
pub struct FinderCache {
  pub(crate) candidates: CandidateSets,
}

#[derive(Debug, Default)]
pub struct FlexImpls {
  pub inv: Inverted,
  pub fork: Option<TirImpl>,
  pub drop: Option<TirImpl>,
  pub self_dual: bool,
}

const STEPS_LIMIT: u32 = 2_000;

#[derive(Debug, Clone, Copy)]
pub(crate) enum Error {
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
    cache: &'a mut FinderCache,
    source: DefId,
    generics: GenericsId,
    span: Span,
  ) -> Self {
    Finder { chart, sigs, diags, cache, source, generics, span, steps: 0 }
  }

  pub fn find_method(
    &mut self,
    types: &Types,
    receiver: Type,
    name: Ident,
  ) -> Result<Vec<(FnId, TypeCtx<Vec<Type>>)>, ErrorGuaranteed> {
    let mut found = Vec::new();

    for candidate in self.find_method_candidates(types, receiver, name)? {
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
          found.push((candidate, TypeCtx { types, inner: type_params }));
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
  ) -> Result<BTreeSet<FnId>, ErrorGuaranteed> {
    let mut found = BTreeSet::new();
    self.get_candidates(&mut found, self.general_candidates().methods.get(&name));

    if let Some(def_id) = types.get_mod(self.chart, receiver)? {
      self.get_candidates(&mut found, self.cache.candidates.get_within(def_id).methods.get(&name));
    }

    Ok(found)
  }

  pub fn find_flex(&mut self, types: &mut Types, ty: Type) -> Result<FlexImpls, ErrorGuaranteed> {
    self._find_flex(types, ty).map_err(|diag| self.diags.error(diag))
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
        error = Err(self.diags.error(diag));
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
      .unwrap_or_else(|err| TirImpl::Error(self.diags.error(err)))
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

  pub(crate) fn find_impl_params(
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
    let mut found = BTreeSet::new();

    if let ImplType::Trait(trait_id, params) = query {
      let key = &(*trait_id, basic);
      self.get_candidates(&mut found, self.general_candidates().impls.get(key));

      let trait_def = self.chart.traits[*trait_id].def;
      self.get_candidates(&mut found, self.cache.candidates.get_within(trait_def).impls.get(key));

      for &param in params {
        if let Some(mod_id) = types.get_mod(self.chart, param)? {
          self.get_candidates(&mut found, self.cache.candidates.get_within(mod_id).impls.get(key));
        }
      }
    }

    Ok(found)
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
          self.find_auto_impls_fn(type_params, types, found)?;
        }
        if Some(*trait_id) == self.chart.builtins.fork {
          self.find_auto_impls_fork(type_params, types, found);
        }
        if Some(*trait_id) == self.chart.builtins.drop {
          self.find_auto_impls_drop(type_params, types, found);
        }
        if Some(*trait_id) == self.chart.builtins.tuple {
          self.find_auto_impls_tuple(type_params, types, found);
        }
        if Some(*trait_id) == self.chart.builtins.object {
          self.find_auto_impls_object(type_params, types, found);
        }
        if Some(*trait_id) == self.chart.builtins.struct_ {
          self.find_auto_impls_struct(type_params, types, found);
        }
        if Some(*trait_id) == self.chart.builtins.enum_ {
          self.find_auto_impls_enum(type_params, types, found);
        }
        if Some(*trait_id) == self.chart.builtins.if_const {
          self.find_auto_impls_if_const(type_params, types, found);
        }
      }
      ImplType::Error(_) => {}
    }
    Ok(())
  }

  fn general_candidates(&self) -> &candidates::CandidateSet {
    self.cache.candidates.get_within_ancestors(self.source)
  }

  fn get_candidates<T: Copy + Hash + Ord>(
    &self,
    set: &mut BTreeSet<T>,
    map: Option<&HashMap<T, VisSet>>,
  ) {
    if let Some(map) = map {
      let ancestors = &self.chart.defs[self.source].ancestors;
      for (&key, vis) in map {
        if let VisSet::Defs(defs) = vis
          && !ancestors.iter().any(|x| defs.contains(x))
        {
          continue;
        }

        set.insert(key);
      }
    }
  }
}
