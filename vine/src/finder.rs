use std::collections::{BTreeSet, HashSet};

use crate::{
  ast::{Generics, Ident, Impl, ImplKind, Span},
  chart::{Chart, Def, DefId, GenericsId, ImplDefId, MemberKind, ValueDefId},
  checker::{ChartTypes, Type},
  unifier::{Unifier, UnifierCheckpoint},
};

pub struct Finder<'core, 'a> {
  pub(crate) chart: &'a Chart<'core>,
  pub(crate) initial_checkpoint: UnifierCheckpoint,
  pub(crate) unifier: &'a mut Unifier<'core>,
  pub(crate) types: &'a ChartTypes<'core>,
  pub(crate) span: Span,
  pub(crate) source: DefId,
  pub(crate) generics: GenericsId,
  pub(crate) steps: u32,
}

struct CandidateSearch<F> {
  modules: HashSet<DefId>,
  consider_candidate: F,
}

const STEPS_LIMIT: u32 = 1_000;

#[derive(Debug, Default, Clone, Copy)]
pub struct Timeout;

impl<'core> Finder<'core, '_> {
  pub fn find_method(
    &mut self,
    receiver: &Type<'core>,
    name: Ident,
  ) -> Vec<(ValueDefId, Vec<Type<'core>>)> {
    let mut found = Vec::new();

    for candidate in self.find_method_candidates(receiver, name) {
      let checkpoint = self.unifier.checkpoint();
      let generics = self.chart.values[candidate].generics;
      let mut type_params = (0..self.chart.generics[generics].type_params.len())
        .map(|_| self.unifier.new_var(self.span))
        .collect::<Vec<_>>();
      if let Some(ty) = self.types.value_types[candidate].receiver() {
        let mut ty = ty.instantiate(&type_params);
        if self.unifier.unify(&mut ty, &mut receiver.clone()) {
          type_params.iter_mut().for_each(|t| self.unifier.export(&checkpoint, t));
          found.push((candidate, type_params));
        }
      }
      self.unifier.revert(&checkpoint);
    }

    found
  }

  fn find_method_candidates(
    &mut self,
    receiver: &Type<'core>,
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

    if let Ok(Some(def_id)) = receiver.get_mod(self.chart) {
      self.find_candidates_within(def_id, search);
    }

    candidates
  }

  pub fn find_impl(
    &mut self,
    query: &Type<'core>,
  ) -> Result<Vec<(Impl<'core>, Type<'core>)>, Timeout> {
    self.step()?;

    let mut found = Vec::new();

    for (i, ty) in self.types.impl_param_types[self.generics].iter().enumerate() {
      let checkpoint = self.unifier.checkpoint();
      let mut ty = ty.clone();
      if self.unifier.unify(&mut ty, &mut query.clone()) {
        self.unifier.export(&checkpoint, &mut ty);
        found.push((Impl { span: self.span, kind: ImplKind::Param(i) }, ty));
      }
      self.unifier.revert(&checkpoint);
    }

    for candidate in self.find_impl_candidates(query) {
      let checkpoint = self.unifier.checkpoint();
      let generics = self.chart.impls[candidate].generics;
      let type_params = (0..self.chart.generics[generics].type_params.len())
        .map(|_| self.unifier.new_var(self.span))
        .collect::<Vec<_>>();
      let mut ty = self.types.impl_def_types[candidate].instantiate(&type_params);
      if self.unifier.unify(&mut ty, &mut query.clone()) {
        let queries = self.types.impl_param_types[generics]
          .iter()
          .map(|t| t.instantiate(&type_params))
          .collect::<Vec<_>>();
        let results = self._find_impls(&checkpoint, &ty, &queries)?;
        for (mut subimpls, ty) in results {
          subimpls.reverse();
          found.push((
            Impl {
              span: self.span,
              kind: ImplKind::Def(
                candidate,
                Generics { span: self.span, types: Vec::new(), impls: subimpls },
              ),
            },
            ty,
          ));
        }
      }
      self.unifier.revert(&checkpoint);
    }

    Ok(found)
  }

  fn _find_impls(
    &mut self,
    root_checkpoint: &UnifierCheckpoint,
    root_ty: &Type<'core>,
    queries: &[Type<'core>],
  ) -> Result<Vec<(Vec<Impl<'core>>, Type<'core>)>, Timeout> {
    let [query, rest_queries @ ..] = queries else {
      let mut root_ty = root_ty.clone();
      self.unifier.export(root_checkpoint, &mut root_ty);
      return Ok(vec![(vec![], root_ty)]);
    };

    let mut found = Vec::new();

    let impls = self.find_impl(query)?;
    for (impl_, mut ty) in impls {
      let checkpoint = self.unifier.checkpoint();
      self.unifier.import(self.span, [&mut ty]);
      let result = self.unifier.unify(&mut ty, &mut query.clone());
      assert!(result);
      let rest = self._find_impls(root_checkpoint, root_ty, rest_queries)?;
      found.extend(rest.into_iter().map(|(mut impls, ty)| {
        impls.push(impl_.clone());
        (impls, ty)
      }));
      self.unifier.revert(&checkpoint);
    }

    Ok(found)
  }

  fn find_impl_candidates(&mut self, query: &Type<'core>) -> BTreeSet<ImplDefId> {
    let Type::Trait(goal_trait_id, _) = query else { return BTreeSet::new() };
    let mut candidates = BTreeSet::new();
    let search = &mut CandidateSearch {
      modules: HashSet::new(),
      consider_candidate: |def: &Def| {
        if let Some(impl_id) = def.impl_def {
          let ty = &self.types.impl_def_types[impl_id];
          if let Type::Trait(trait_id, _) = ty {
            if trait_id == goal_trait_id {
              candidates.insert(impl_id);
            }
          }
        }
      },
    };

    for &ancestor in &self.chart.defs[self.source].ancestors {
      self.find_candidates_within(ancestor, search);
    }

    if let Type::Trait(trait_id, params) = query {
      self.find_candidates_within(self.chart.traits[*trait_id].def, search);
      for param in params {
        let mut param = param.clone();
        self.unifier.try_concretize(&mut param);
        if let Ok(Some(mod_id)) = param.get_mod(self.chart) {
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
          let ty = &self.types.impl_def_types[impl_id];
          if let Type::Trait(trait_id, _) = ty {
            let trait_def = &self.chart.traits[*trait_id];
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
      self.unifier.revert(&self.initial_checkpoint);
      Err(Timeout)
    } else {
      Ok(())
    }
  }
}
