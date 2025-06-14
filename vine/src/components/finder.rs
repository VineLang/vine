use std::collections::{BTreeSet, HashSet};

use crate::structures::{
  ast::{Ident, Span},
  chart::{
    Chart, Def, DefId, DefImplKind, DefTraitKind, DefValueKind, FnId, GenericsId, ImplId,
    MemberKind, WithVis,
  },
  core::Core,
  diag::Diag,
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

struct CandidateSearch<F> {
  modules: HashSet<DefId>,
  consider_candidate: F,
}

const STEPS_LIMIT: u32 = 1_000;

#[derive(Debug, Default, Clone, Copy)]
pub struct Timeout;

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
      let type_params = types.new_vars(self.span, self.chart.generics[generics].type_params.len());
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
      modules: HashSet::new(),
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

  pub fn find_impl(&mut self, types: &mut Types<'core>, query: &ImplType) -> TirImpl {
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
  ) -> Result<Vec<TypeCtx<'core, TirImpl>>, Timeout> {
    self.step()?;

    let mut found = Vec::new();

    let generics = &self.sigs.generics[self.generics];
    for (i, ty) in generics.inner.impl_params.iter().enumerate() {
      if query.approx_eq(ty) {
        let mut types = types.clone();
        let ty = types.import_with(generics, None, |t, _| t.transfer(ty));
        if types.unify_impl_type(&ty, query).is_success() {
          found.push(TypeCtx { types, inner: TirImpl::Param(i) });
        }
      }
    }

    for candidate in self.find_impl_candidates(types, query) {
      let mut types = types.clone();
      let generics = self.chart.impls[candidate].generics;
      let type_params = (0..self.chart.generics[generics].type_params.len())
        .map(|_| types.new_var(self.span))
        .collect::<Vec<_>>();
      let ty = types.import(&self.sigs.impls[candidate], Some(&type_params)).ty;
      if types.unify_impl_type(&ty, query).is_success() {
        let queries = types.import(&self.sigs.generics[generics], Some(&type_params)).impl_params;
        let results = self.find_subimpls(types, &queries)?;
        for result in results {
          let mut impls = result.inner;
          impls.reverse();
          found.push(TypeCtx { types: result.types, inner: TirImpl::Def(candidate, impls) });
        }
      }
    }

    Ok(found)
  }

  fn find_subimpls(
    &mut self,
    types: Types<'core>,
    queries: &[ImplType],
  ) -> Result<Vec<TypeCtx<'core, Vec<TirImpl>>>, Timeout> {
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
      modules: HashSet::new(),
      consider_candidate: |self_: &Self, def: &Def| {
        if let Some(WithVis { vis, kind: DefImplKind::Impl(impl_id) }) = def.impl_kind {
          if self_.chart.visible(vis, self_.source) {
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

    for member in self.chart.defs[mod_id].members.values() {
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
}
