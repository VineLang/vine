use std::{
  collections::{hash_map::Entry, HashMap},
  mem::take,
};

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  ast::{Expr, ExprKind, Impl, ImplKind},
  chart::{Chart, ChartCheckpoint, ImplDefId, ImplDefKind, SubitemId, ValueDefId, ValueDefKind},
  visit::{VisitMut, Visitee},
};

#[derive(Debug)]
pub struct Specializer<'core, 'a> {
  pub specs: &'a mut Specializations<'core>,
  pub chart: &'a mut Chart<'core>,
}

#[derive(Debug, Default)]
pub struct Specializations<'core> {
  defs: IdxVec<ValueDefId, ValueDefInfo<'core>>,
  pub specs: IdxVec<SpecId, Option<Spec>>,
}

impl<'core, 'a> Specializer<'core, 'a> {
  pub fn specialize_since(&mut self, checkpoint: &ChartCheckpoint) {
    self.initialize(checkpoint);
    self.specialize_roots(checkpoint);
  }

  fn initialize(&mut self, checkpoint: &ChartCheckpoint) {
    for value_id in self.chart.values.keys_from(checkpoint.values) {
      let value_def = &mut self.chart.values[value_id];
      let def_info = {
        let impl_params = self.chart.generics[value_def.generics].impl_params.len();
        let mut kind = take(&mut value_def.kind);
        let mut extractor = RelExtractor { rels: IdxVec::new(), chart: self.chart };
        match &mut kind {
          ValueDefKind::Taken => unreachable!(),
          ValueDefKind::Const { value, .. } => extractor.visit_expr(value),
          ValueDefKind::Fn { body, .. } => extractor.visit_block(body),
          ValueDefKind::Ivy { .. } | ValueDefKind::Adt(..) | ValueDefKind::TraitSubitem(..) => {}
        };
        let rels = extractor.rels;
        self.chart.values[value_id].kind = kind;
        ValueDefInfo { impl_params, rels, specs: HashMap::new() }
      };
      self.specs.defs.push(def_info);
    }
  }

  fn specialize_roots(&mut self, checkpoint: &ChartCheckpoint) {
    for def_id in self.specs.defs.keys_from(checkpoint.values) {
      if self.specs.defs[def_id].impl_params == 0 {
        let id = self.specialize(def_id, Vec::new());
        self.specs.specs[id].as_mut().unwrap().singular = true;
      }
    }
  }

  fn specialize(&mut self, def_id: ValueDefId, impl_args: Vec<ImplTree>) -> SpecId {
    let def = &mut self.specs.defs[def_id];
    let entry = match def.specs.entry(impl_args) {
      Entry::Occupied(e) => return *e.get(),
      Entry::Vacant(e) => e,
    };
    let spec_id = self.specs.specs.push(None);
    let impl_args = entry.key().clone();
    entry.insert(spec_id);
    let index = def.specs.len();
    let rels = &def.rels;
    let mut spec_rels = IdxVec::new();
    for rel_id in rels.keys() {
      let rel = &self.specs.defs[def_id].rels[rel_id];
      let (rel_def_id, rel_impl_args) = self.instantiate_rel(rel, &impl_args);
      let specialized = self.specialize(rel_def_id, rel_impl_args);
      spec_rels.push(specialized);
    }
    let spec = Spec { value: def_id, index, singular: false, rels: spec_rels };
    self.specs.specs[spec_id] = Some(spec);
    spec_id
  }

  fn instantiate_rel(
    &self,
    rel: &Rel<'core>,
    impl_args: &[ImplTree],
  ) -> (ValueDefId, Vec<ImplTree>) {
    match rel {
      Rel::Def(rel_def_id, impls) => {
        (*rel_def_id, impls.iter().map(|x| instantiate(x, impl_args)).collect())
      }
      Rel::Subitem(impl_, subitem_id) => {
        let impl_ = instantiate(impl_, impl_args);
        let value = match &self.chart.impls[impl_.0].kind {
          ImplDefKind::Taken => unreachable!(),
          ImplDefKind::Impl { subitems, .. } => subitems[*subitem_id].value,
        };
        (value, impl_.1)
      }
    }
  }

  pub(crate) fn _specialize_custom<'t>(
    &mut self,
    visitee: impl Visitee<'core, 't>,
  ) -> IdxVec<RelId, SpecId> {
    let mut extractor = RelExtractor { rels: IdxVec::new(), chart: self.chart };
    extractor.visit(visitee);
    extractor
      .rels
      .into_values()
      .map(|rel| {
        let (rel_def_id, rel_impl_args) = self.instantiate_rel(&rel, &Vec::new());
        self.specialize(rel_def_id, rel_impl_args)
      })
      .collect::<Vec<_>>()
      .into()
  }
}

struct RelExtractor<'core, 'a> {
  rels: IdxVec<RelId, Rel<'core>>,
  chart: &'a Chart<'core>,
}

impl<'core> VisitMut<'core, '_> for RelExtractor<'core, '_> {
  fn visit_expr(&mut self, expr: &mut Expr<'core>) {
    if let ExprKind::Def(value_id, generics) = &mut expr.kind {
      if !generics.impls.is_empty() {
        let mut impls = take(&mut generics.impls);
        let rel = if let ValueDefKind::TraitSubitem(_, id) = self.chart.values[*value_id].kind {
          Rel::Subitem(impls.pop().unwrap(), id)
        } else {
          Rel::Def(*value_id, impls)
        };
        let rel_id = self.rels.push(rel);
        expr.kind = ExprKind::Rel(rel_id);
      }
    }
    self._visit_expr(expr);
  }
}

#[derive(Debug, Default)]
struct ValueDefInfo<'core> {
  impl_params: usize,
  rels: IdxVec<RelId, Rel<'core>>,
  specs: HashMap<Vec<ImplTree>, SpecId>,
}

new_idx!(pub RelId);
new_idx!(pub SpecId);
new_idx!(pub ImplId);

#[derive(Debug)]
pub struct Spec {
  pub value: ValueDefId,
  pub index: usize,
  pub singular: bool,
  pub rels: IdxVec<RelId, SpecId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ImplTree(ImplDefId, Vec<ImplTree>);

#[derive(Debug)]
enum Rel<'core> {
  Def(ValueDefId, Vec<Impl<'core>>),
  Subitem(Impl<'core>, SubitemId),
}

fn instantiate<'core>(impl_: &Impl<'core>, params: &[ImplTree]) -> ImplTree {
  match &impl_.kind {
    ImplKind::Hole | ImplKind::Path(_) | ImplKind::Error(_) => unreachable!(),
    ImplKind::Param(i) => params[*i].clone(),
    ImplKind::Def(id, generics) => {
      ImplTree(*id, generics.impls.iter().map(|i| instantiate(i, params)).collect())
    }
  }
}

impl<'core> Specializations<'core> {
  pub fn revert(&mut self, checkpoint: &ChartCheckpoint, specs: SpecId) {
    self.specs.truncate(specs.0);
    self.defs.truncate(checkpoint.values.0);
    self.defs.values_mut().for_each(|info| info.specs.retain(|_, s| *s < specs));
  }
}
