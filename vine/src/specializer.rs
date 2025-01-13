use std::{
  collections::{hash_map::Entry, HashMap},
  mem::take,
};

use vine_util::{idx::IdxVec, new_idx, unwrap_idx_vec};

use crate::{
  ast::{Expr, ExprKind, Ident, Impl, ImplKind},
  resolver::{DefId, Resolver, ValueDefKind},
  visit::VisitMut,
};

pub fn specialize(resolver: &mut Resolver) -> IdxVec<DefId, IdxVec<SpecId, Spec>> {
  let mut specializer = Specializer { defs: IdxVec::new(), resolver };
  specializer.initialize();
  specializer.specialize_roots();
  specializer.defs.into_iter().map(|d| unwrap_idx_vec(d.1.specs)).collect::<Vec<_>>().into()
}

#[derive(Debug)]
struct Specializer<'core, 'a> {
  defs: IdxVec<DefId, DefInfo<'core>>,
  resolver: &'a mut Resolver<'core>,
}

impl<'core, 'a> Specializer<'core, 'a> {
  fn initialize(&mut self) {
    for def_id in self.resolver.defs.keys() {
      let def = &mut self.resolver.defs[def_id];
      let def_info = if let Some(mut value_def) = def.value_def.take() {
        let impl_params = value_def.impl_params.len();
        let mut extractor = RelExtractor { rels: IdxVec::new(), resolver: self.resolver };
        if let ValueDefKind::Expr(expr) = &mut value_def.kind {
          extractor.visit_expr(expr);
        }
        let rels = extractor.rels;
        self.resolver.defs[def_id].value_def = Some(value_def);
        DefInfo { impl_params, rels, specs_lookup: HashMap::new(), specs: IdxVec::new() }
      } else {
        DefInfo::default()
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

  fn specialize(&mut self, def_id: DefId, impl_args: Vec<ImplTree>) -> SpecId {
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
        Rel::Subitem(impl_, name) => {
          let impl_ = instantiate(impl_, &impl_args);
          let subitem_id = self.resolver.defs[impl_.0]
            .impl_def
            .as_ref()
            .unwrap()
            .subitems
            .iter()
            .find(|(_, n, _)| n == name)
            .unwrap()
            .2;
          (subitem_id, impl_.1)
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
  resolver: &'a Resolver<'core>,
}

impl<'core> VisitMut<'core, '_> for RelExtractor<'core, '_> {
  fn visit_expr(&mut self, expr: &mut Expr<'core>) {
    if let ExprKind::Path(path) = &mut expr.kind {
      if let Some(generics) = &mut path.generics {
        if !generics.impls.is_empty() {
          let def_id = path.path.resolved.unwrap();
          let mut impls = take(&mut generics.impls);
          let rel = if let ValueDefKind::TraitSubitem(_, name) =
            self.resolver.defs[def_id].value_def.as_ref().unwrap().kind
          {
            Rel::Subitem(impls.pop().unwrap(), name)
          } else {
            Rel::Def(def_id, impls)
          };
          let rel_id = self.rels.push(rel);
          expr.kind = ExprKind::Rel(rel_id);
        }
      }
    }
    self._visit_expr(expr);
  }
}

#[derive(Debug, Default)]
struct DefInfo<'core> {
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
  pub rels: IdxVec<RelId, (DefId, SpecId, bool)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ImplTree(DefId, Vec<ImplTree>);

#[derive(Debug)]
enum Rel<'core> {
  Def(DefId, Vec<Impl<'core>>),
  Subitem(Impl<'core>, Ident<'core>),
}

fn instantiate<'core>(impl_: &Impl<'core>, params: &[ImplTree]) -> ImplTree {
  match &impl_.kind {
    ImplKind::Hole | ImplKind::Error(_) => unreachable!(),
    ImplKind::Param(i) => params[*i].clone(),
    ImplKind::Path(p) => ImplTree(
      p.path.resolved.unwrap(),
      p.generics.iter().flat_map(|x| &x.impls).map(|i| instantiate(i, params)).collect(),
    ),
  }
}
