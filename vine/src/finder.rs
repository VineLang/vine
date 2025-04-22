use std::{
  collections::{BTreeSet, HashSet},
  sync::Arc,
};

use crate::{
  ast::{Generics, Ident, Impl, ImplKind, Span},
  chart::{Chart, Def, DefId, GenericsId, ImplDefId, MemberKind, ValueDefId},
  core::Core,
  diag::Diag,
  signatures::Signatures,
  tir::TirImpl,
  types::{ImplType, Type, Types},
};

#[allow(clippy::too_many_arguments)]
pub fn find_impl<'core>(
  types: &mut Types<'core>,
  core: &Core<'core>,
  chart: &Chart<'core>,
  sigs: &Signatures<'core>,
  source: DefId,
  generics: GenericsId,
  span: Span,
  query: &ImplType,
) -> TirImpl {
  let mut export = types.export();
  let sub_query = export.transfer_impl_type(query);
  let sub_types = export.finish();

  let show_ty = || types.show_impl_type(chart, query);
  let mut finder = Finder { chart, sigs, source, generics, steps: 0 };
  let Ok(mut results) = finder.find_impl(&sub_types, &sub_query) else {
    return TirImpl::Error(core.report(Diag::SearchLimit { span, ty: show_ty() }));
  };

  if results.is_empty() {
    return TirImpl::Error(core.report(Diag::CannotFindImpl { span, ty: show_ty() }));
  }

  if results.len() > 1 {
    return TirImpl::Error(core.report(Diag::AmbiguousImpl { span, ty: show_ty() }));
  }

  let result = results.pop().unwrap();

  let mut import = types.import(&result.types, None);
  let result_ty = import.transfer_impl_type(&result.ty);
  let unify_result = types.unify_impl_type(query, &result_ty);
  assert!(unify_result.is_success());

  result.impl_
}

struct Finder<'core, 'a> {
  chart: &'a Chart<'core>,
  sigs: &'a Signatures<'core>,
  source: DefId,
  generics: GenericsId,
  steps: u32,
}

struct CandidateSearch<F> {
  modules: HashSet<DefId>,
  consider_candidate: F,
}

const STEPS_LIMIT: u32 = 1_000;

#[derive(Debug, Default, Clone, Copy)]
pub struct Timeout;

pub struct ImplResult<'core> {
  pub types: Types<'core>,
  pub impl_: TirImpl,
  pub ty: ImplType,
}

struct SubimplsResult<'core> {
  types: Types<'core>,
  impls: Vec<TirImpl>,
}

impl<'core> Finder<'core, '_> {
  // pub fn find_method(
  //   &mut self,
  //   types: &Types<'core>,
  //   receiver: Type,
  //   name: Ident,
  // ) -> Vec<(ValueDefId, Vec<Type>)> {
  //   let mut found = Vec::new();

  //   for candidate in self.find_method_candidates(types, receiver, name) {
  //     let generics = self.chart.values[candidate].generics;
  //     let mut type_params =
  // (0..self.chart.generics[generics].type_params.len())       .map(|_|
  // self.unifier.new_var(self.span))       .collect::<Vec<_>>();
  //     if let Some(ty) = self.sigs.value_types[candidate].receiver() {
  //       let mut ty = ty.instantiate(&type_params);
  //       if self.unifier.unify(&mut ty, &mut receiver.clone()) {
  //         type_params.iter_mut().for_each(|t| self.unifier.export(&checkpoint,
  // t));         found.push((candidate, type_params));
  //       }
  //     }
  //   }

  //   found
  // }

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

  pub fn find_impl(
    &mut self,
    types: &Types<'core>,
    query: &ImplType,
  ) -> Result<Vec<ImplResult<'core>>, Timeout> {
    self.step()?;

    let mut found = Vec::new();

    for (i, ty) in self.sigs.generics[self.generics].impl_params.iter().enumerate() {
      let mut types = types.clone();
      if types.unify_impl_type(&ty, &query).is_success() {
        found.push(ImplResult { types, ty: ty.clone(), impl_: TirImpl::Param(i) });
      }
    }

    for candidate in self.find_impl_candidates(&types, &query) {
      let mut types = types.clone();
      let generics = self.chart.impls[candidate].generics;
      let type_params = (0..self.chart.generics[generics].type_params.len())
        .map(|_| types.new_var())
        .collect::<Vec<_>>();
      let sig = &self.sigs.impls[candidate];
      let ty = types.import(&sig.types, Some(&type_params)).transfer_impl_type(&sig.ty);
      if types.unify_impl_type(&ty, &query).is_success() {
        let sig = &self.sigs.generics[generics];
        let mut import = types.import(&sig.types, Some(&type_params));
        let queries = self.sigs.generics[generics]
          .impl_params
          .iter()
          .map(|t| import.transfer_impl_type(t))
          .collect::<Vec<_>>();
        let results = self.find_subimpls(types, &queries)?;
        for mut result in results {
          result.impls.reverse();
          found.push(ImplResult {
            types: result.types,
            impl_: TirImpl::Def(candidate, result.impls),
            ty: ty.clone(),
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
  ) -> Result<Vec<SubimplsResult<'core>>, Timeout> {
    let [query, rest_queries @ ..] = queries else {
      return Ok(vec![(SubimplsResult { types, impls: vec![] })]);
    };

    let mut found = Vec::new();

    let impls = self.find_impl(&types, query)?;
    for impl_result in impls {
      let rest = self.find_subimpls(impl_result.types, rest_queries)?;
      found.extend(rest.into_iter().map(|mut subimpls_result| {
        subimpls_result.impls.push(impl_result.impl_.clone());
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
          if impl_sig.ty.approx_eq(query) {
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
          if let ImplType::Trait(trait_id, _) = impl_sig.ty {
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
