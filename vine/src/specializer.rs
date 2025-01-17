use std::{
  collections::{hash_map::Entry, HashMap},
  mem::take,
};

use vine_util::{idx::IdxVec, new_idx, unwrap_idx_vec};

use crate::{
  ast::{Expr, ExprKind, Impl, ImplKind},
  chart::{Chart, ImplDefId, ImplDefKind, SubitemId, ValueDefId, ValueDefKind},
  visit::VisitMut,
};

pub fn specialize(chart: &mut Chart) -> IdxVec<ValueDefId, IdxVec<SpecId, Spec>> {
  let mut specializer = Specializer { defs: IdxVec::new(), chart };
  specializer.initialize();
  specializer.specialize_roots();
  specializer.defs.into_iter().map(|d| unwrap_idx_vec(d.1.specs)).collect::<Vec<_>>().into()
}

#[derive(Debug)]
struct Specializer<'core, 'a> {
  defs: IdxVec<ValueDefId, ValueDefInfo<'core>>,
  chart: &'a mut Chart<'core>,
}

impl<'core, 'a> Specializer<'core, 'a> {
  fn initialize(&mut self) {
    for value_id in self.chart.values.keys() {
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
        ValueDefInfo { impl_params, rels, specs_lookup: HashMap::new(), specs: IdxVec::new() }
      };
      self.defs.push(def_info);
    }
  }

  fn specialize_roots(&mut self) {
    for def_id in self.defs.keys() {
      if self.defs[def_id].impl_params == 0 {
        self.specialize(def_id, Vec::new());
        self.defs[def_id].specs[SpecId(0)].as_mut().unwrap().singular = true;
      }
    }
  }

  fn specialize(&mut self, def_id: ValueDefId, impl_args: Vec<ImplTree>) -> SpecId {
    let def = &mut self.defs[def_id];
    let entry = match def.specs_lookup.entry(impl_args) {
      Entry::Occupied(e) => return *e.get(),
      Entry::Vacant(e) => e,
    };
    let spec_id = def.specs.push(None);
    let impl_args = entry.key().clone();
    entry.insert(spec_id);
    let mut spec = Spec::default();
    for rel_id in def.rels.keys() {
      let rel = &self.defs[def_id].rels[rel_id];
      let (rel_def_id, rel_impl_args) = match rel {
        Rel::Def(rel_def_id, impls) => {
          (*rel_def_id, impls.iter().map(|x| instantiate(x, &impl_args)).collect())
        }
        Rel::Subitem(impl_, subitem_id) => {
          let impl_ = instantiate(impl_, &impl_args);
          let value = match &self.chart.impls[impl_.0].kind {
            ImplDefKind::Taken => unreachable!(),
            ImplDefKind::Impl { subitems, .. } => subitems[*subitem_id].value,
          };
          (value, impl_.1)
        }
      };
      let singular = rel_impl_args.is_empty();
      let rel_spec_id = self.specialize(rel_def_id, rel_impl_args);
      spec.rels.push((rel_def_id, rel_spec_id, singular));
    }
    self.defs[def_id].specs[spec_id] = Some(spec);
    spec_id
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
  specs_lookup: HashMap<Vec<ImplTree>, SpecId>,
  specs: IdxVec<SpecId, Option<Spec>>,
}

new_idx!(pub RelId);
new_idx!(pub SpecId);
new_idx!(pub ImplId);

#[derive(Debug, Default)]
pub struct Spec {
  pub singular: bool,
  pub rels: IdxVec<RelId, (ValueDefId, SpecId, bool)>,
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
