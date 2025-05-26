use std::collections::{BTreeSet, HashSet};

use crate::{
  ast::{GenericArgs, Ident, Impl, ImplKind, Span},
  chart::{Chart, Def, DefId, GenericsId, ImplDefId, MemberKind, ValueDefId},
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  signatures::Signatures,
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
  ) -> Vec<(ValueDefId, TypeCtx<'core, Vec<Type>>)> {
    let mut found = Vec::new();

    for candidate in self.find_method_candidates(types, receiver, name) {
      let mut types = types.clone();
      let generics = self.chart.values[candidate].generics;
      let type_params = (0..self.chart.generics[generics].type_params.len())
        .map(|_| types.new_var(self.span))
        .collect::<Vec<_>>();
      let candidate_ty =
        types.import(&self.sigs.values[candidate], Some(&type_params), |t, sig| t.transfer(sig.ty));
      if let Some((Inverted(false), TypeKind::Fn(params, _))) = types.kind(candidate_ty) {
        if let [candidate_receiver, ..] = **params {
          let candidate_receiver = match types.kind(candidate_receiver) {
            Some((Inverted(false), TypeKind::Ref(t))) => *t,
            _ => candidate_receiver,
          };
          if types.unify(receiver, candidate_receiver).is_success() {
            found.push((candidate, TypeCtx { types, inner: type_params }));
          }
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
  ) -> BTreeSet<ValueDefId> {
    let mut candidates = BTreeSet::new();
    let search = &mut CandidateSearch {
      modules: HashSet::new(),
      consider_candidate: |def: &Def| {
        if def.name == name {
          if let Some(value_id) = def.value_def {
            let value = &self.chart.values[value_id];
            if value.method {
              candidates.insert(value_id);
            }
          }
        }
      },
    };

    for &ancestor in &self.chart.defs[self.source].ancestors {
      self.find_candidates_within(ancestor, search);
    }

    if let Some(def_id) = types.get_mod(self.chart, receiver) {
      self.find_candidates_within(def_id, search);
    }

    candidates
  }

  pub fn find_impl(&mut self, types: &mut Types<'core>, query: &ImplType) -> Impl<'core> {
    let span = self.span;
    let TypeCtx { types: sub_types, inner: sub_query } =
      types.export(|t| t.transfer_impl_type(query));

    let show_ty = || types.show_impl_type(self.chart, query);
    let Ok(mut results) = self._find_impl(&sub_types, &sub_query) else {
      return impl_error(span, self.core.report(Diag::SearchLimit { span, ty: show_ty() }));
    };

    if results.is_empty() {
      return impl_error(span, self.core.report(Diag::CannotFindImpl { span, ty: show_ty() }));
    }

    if results.len() > 1 {
      return impl_error(span, self.core.report(Diag::AmbiguousImpl { span, ty: show_ty() }));
    }

    let result = results.pop().unwrap();

    let result_ty = types.import(&result, None, |t, _| t.transfer_impl_type(&sub_query));
    let unify_result = types.unify_impl_type(query, &result_ty);
    assert!(!unify_result.is_failure());

    result.inner
  }

  fn _find_impl(
    &mut self,
    types: &Types<'core>,
    query: &ImplType,
  ) -> Result<Vec<TypeCtx<'core, Impl<'core>>>, Timeout> {
    self.step()?;

    let mut found = Vec::new();

    let generics = &self.sigs.generics[self.generics];
    for (i, ty) in generics.inner.impl_params.iter().enumerate() {
      if query.approx_eq(ty) {
        let mut types = types.clone();
        let ty = types.import(generics, None, |t, _| t.transfer_impl_type(ty));
        if types.unify_impl_type(&ty, query).is_success() {
          found.push(TypeCtx { types, inner: Impl { span: self.span, kind: ImplKind::Param(i) } });
        }
      }
    }

    for candidate in self.find_impl_candidates(types, query) {
      let mut types = types.clone();
      let generics = self.chart.impls[candidate].generics;
      let type_params = (0..self.chart.generics[generics].type_params.len())
        .map(|_| types.new_var(self.span))
        .collect::<Vec<_>>();
      let ty = types.import(&self.sigs.impls[candidate], Some(&type_params), |t, sig| {
        t.transfer_impl_type(&sig.ty)
      });
      if types.unify_impl_type(&ty, query).is_success() {
        let queries = types.import(&self.sigs.generics[generics], Some(&type_params), |t, sig| {
          sig.impl_params.iter().map(|ty| t.transfer_impl_type(ty)).collect::<Vec<_>>()
        });
        let results = self.find_subimpls(types, &queries)?;
        for result in results {
          let mut impls = result.inner;
          impls.reverse();
          found.push(TypeCtx {
            types: result.types,
            inner: Impl {
              span: self.span,
              kind: ImplKind::Def(candidate, GenericArgs { span: self.span, types: vec![], impls }),
            },
          });
        }
      }
    }

    Ok(found)
  }

  fn find_subimpls(
    &mut self,
    types: Types<'core>,
    queries: &[ImplType],
  ) -> Result<Vec<TypeCtx<'core, Vec<Impl<'core>>>>, Timeout> {
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

  fn find_impl_candidates(
    &mut self,
    types: &Types<'core>,
    query: &ImplType,
  ) -> BTreeSet<ImplDefId> {
    let mut candidates = BTreeSet::new();
    let search = &mut CandidateSearch {
      modules: HashSet::new(),
      consider_candidate: |def: &Def| {
        if let Some(impl_id) = def.impl_def {
          let impl_sig = &self.sigs.impls[impl_id];
          if impl_sig.inner.ty.approx_eq(query) {
            candidates.insert(impl_id);
          }
        }
      },
    };

    for &ancestor in &self.chart.defs[self.source].ancestors {
      self.find_candidates_within(ancestor, search);
    }

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

  fn find_candidates_within<F: FnMut(&Def)>(
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
        MemberKind::Import(import_id) => self.chart.imports[import_id].resolved(),
      };
      let Some(def_id) = def_id else { continue };

      let def = &self.chart.defs[def_id];

      (search.consider_candidate)(def);

      if let Some(trait_id) = def.trait_def {
        let trait_def = &self.chart.traits[trait_id];
        if self.chart.visible(trait_def.vis, self.source) {
          self.find_candidates_within(trait_def.def, search);
        }
      }

      if let Some(impl_id) = def.impl_def {
        let impl_def = &self.chart.impls[impl_id];
        if self.chart.visible(impl_def.vis, self.source) {
          let impl_sig = &self.sigs.impls[impl_id];
          if let ImplType::Trait(trait_id, _) = impl_sig.inner.ty {
            let trait_def = &self.chart.traits[trait_id];
            if self.chart.visible(trait_def.vis, self.source) {
              self.find_candidates_within(trait_def.def, search);
            }
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

fn impl_error<'core>(span: Span, err: ErrorGuaranteed) -> Impl<'core> {
  Impl { span, kind: ImplKind::Error(err) }
}
