use std::{
  collections::{hash_map::Entry, HashMap},
  mem::take,
};

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  ast::{Expr, ExprKind, Impl, ImplKind},
  chart::{
    checkpoint::ChartCheckpoint, Chart, ConcreteConstId, ConcreteFnId, ConstId, DefId, FnId,
    GenericsDef, ImplId, TraitConstId, TraitFnId,
  },
  diag::ErrorGuaranteed,
  visit::{VisitMut, Visitee},
};

#[derive(Debug)]
pub struct Specializer<'core, 'a> {
  pub specs: &'a mut Specializations<'core>,
  pub chart: &'a mut Chart<'core>,
}

#[derive(Debug, Default)]
pub struct Specializations<'core> {
  consts: IdxVec<ConcreteConstId, TemplateInfo<'core>>,
  fns: IdxVec<ConcreteFnId, TemplateInfo<'core>>,
  pub specs: IdxVec<SpecId, Option<Spec>>,
}

impl<'core, 'a> Specializer<'core, 'a> {
  pub fn specialize_since(&mut self, checkpoint: &ChartCheckpoint) {
    self.initialize(checkpoint);
    self.specialize_roots(checkpoint);
  }

  fn initialize(&mut self, checkpoint: &ChartCheckpoint) {
    for const_id in self.chart.concrete_consts.keys_from(checkpoint.concrete_consts) {
      let mut const_def = self.chart.concrete_consts[const_id].clone();
      let mut extractor = RelExtractor::default();
      extractor.template.impl_params = impl_param_count(&self.chart.generics[const_def.generics]);
      extractor.visit_expr(&mut const_def.value);
      self.specs.consts.push(extractor.template);
      self.chart.concrete_consts[const_id] = const_def;
    }

    for fn_id in self.chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      let mut fn_def = self.chart.concrete_fns[fn_id].clone();
      let mut extractor = RelExtractor::default();
      extractor.template.impl_params = impl_param_count(&self.chart.generics[fn_def.generics]);
      extractor.visit_block(&mut fn_def.body);
      self.specs.fns.push(extractor.template);
      self.chart.concrete_fns[fn_id] = fn_def;
    }
  }

  fn specialize_roots(&mut self, checkpoint: &ChartCheckpoint) {
    for const_id in self.specs.consts.keys_from(checkpoint.concrete_consts) {
      if self.specs.consts[const_id].impl_params == 0 {
        self.specialize(TemplateId::Const(const_id), Vec::new());
      }
    }
    for fn_id in self.specs.fns.keys_from(checkpoint.concrete_fns) {
      if self.specs.fns[fn_id].impl_params == 0 {
        self.specialize(TemplateId::Fn(fn_id), Vec::new());
      }
    }
  }

  fn specialize(&mut self, id: TemplateId, impl_args: Vec<ImplTree>) -> SpecId {
    let spec_id = self.specs.specs.next_index();
    let template = &mut self.template_mut(id);
    let index = template.specs.len();
    let entry = match template.specs.entry(impl_args) {
      Entry::Occupied(e) => return *e.get(),
      Entry::Vacant(e) => e,
    };
    let impl_args = entry.key().clone();
    entry.insert(spec_id);
    let fn_rel_ids = template.fn_rels.keys();
    let const_rel_ids = template.const_rels.keys();
    self.specs.specs.push(None);
    let rels = SpecRels {
      fns: IdxVec::from(Vec::from_iter(fn_rel_ids.map(|rel_id| {
        let rel = &self.template(id).fn_rels[rel_id];
        let (fn_id, args) = self.instantiate_fn_rel(rel, &impl_args)?;
        Ok(self.specialize(TemplateId::Fn(fn_id), args))
      }))),
      consts: IdxVec::from(Vec::from_iter(const_rel_ids.map(|rel_id| {
        let rel = &self.template(id).const_rels[rel_id];
        let (const_id, args) = self.instantiate_const_rel(rel, &impl_args)?;
        Ok(self.specialize(TemplateId::Const(const_id), args))
      }))),
    };
    let def = match id {
      TemplateId::Const(id) => self.chart.concrete_consts[id].def,
      TemplateId::Fn(id) => self.chart.concrete_fns[id].def,
    };
    let spec = Spec { def, template: id, index, singular: impl_args.is_empty(), rels };
    self.specs.specs[spec_id] = Some(spec);
    spec_id
  }

  fn instantiate_fn_rel(
    &self,
    rel: &Rel<'core, ConcreteFnId, TraitFnId>,
    impl_args: &[ImplTree],
  ) -> Result<(ConcreteFnId, Vec<ImplTree>), ErrorGuaranteed> {
    match rel {
      Rel::Def(id, impls) => Ok((*id, impls.iter().map(|x| instantiate(x, impl_args)).collect())),
      Rel::Subitem(impl_, trait_fn_id) => {
        let ImplTree(impl_id, impls) = instantiate(impl_, impl_args);
        Ok((self.chart.impls[impl_id].fns[*trait_fn_id]?, impls))
      }
    }
  }

  fn instantiate_const_rel(
    &self,
    rel: &Rel<'core, ConcreteConstId, TraitConstId>,
    impl_args: &[ImplTree],
  ) -> Result<(ConcreteConstId, Vec<ImplTree>), ErrorGuaranteed> {
    match rel {
      Rel::Def(id, impls) => Ok((*id, impls.iter().map(|x| instantiate(x, impl_args)).collect())),
      Rel::Subitem(impl_, trait_const_id) => {
        let ImplTree(impl_id, impls) = instantiate(impl_, impl_args);
        Ok((self.chart.impls[impl_id].consts[*trait_const_id]?, impls))
      }
    }
  }

  pub(crate) fn _specialize_custom<'t>(&mut self, visitee: impl Visitee<'core, 't>) -> SpecRels {
    let mut extractor = RelExtractor::default();
    extractor.visit(visitee);
    let template = extractor.template;
    SpecRels {
      fns: IdxVec::from(Vec::from_iter(template.fn_rels.values().map(|rel| {
        let (fn_id, args) = self.instantiate_fn_rel(rel, &[])?;
        Ok(self.specialize(TemplateId::Fn(fn_id), args))
      }))),
      consts: IdxVec::from(Vec::from_iter(template.const_rels.values().map(|rel| {
        let (const_id, args) = self.instantiate_const_rel(rel, &[])?;
        Ok(self.specialize(TemplateId::Const(const_id), args))
      }))),
    }
  }

  fn template(&self, id: TemplateId) -> &TemplateInfo<'core> {
    match id {
      TemplateId::Const(const_id) => &self.specs.consts[const_id],
      TemplateId::Fn(fn_id) => &self.specs.fns[fn_id],
    }
  }

  fn template_mut(&mut self, id: TemplateId) -> &mut TemplateInfo<'core> {
    match id {
      TemplateId::Const(const_id) => &mut self.specs.consts[const_id],
      TemplateId::Fn(fn_id) => &mut self.specs.fns[fn_id],
    }
  }
}

fn impl_param_count(generics: &GenericsDef) -> usize {
  generics
    .type_params
    .iter()
    .map(|p| p.flex.fork() as usize + p.flex.drop() as usize)
    .sum::<usize>()
    + generics.impl_params.len()
}

#[derive(Debug, Clone, Copy)]
pub enum TemplateId {
  Const(ConcreteConstId),
  Fn(ConcreteFnId),
}

#[derive(Debug, Default)]
struct RelExtractor<'core> {
  template: TemplateInfo<'core>,
}

impl<'core> VisitMut<'core, '_> for RelExtractor<'core> {
  fn visit_expr(&mut self, expr: &mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::ConstDef(const_id, generics) => {
        let mut impls = take(&mut generics.impls);
        expr.kind = ExprKind::ConstRel(self.template.const_rels.push(match *const_id {
          ConstId::Concrete(const_id) => Rel::Def(const_id, impls),
          ConstId::Abstract(_, const_id) => Rel::Subitem(impls.pop().unwrap(), const_id),
        }));
      }
      ExprKind::FnDef(fn_id, generics) => {
        let mut impls = take(&mut generics.impls);
        expr.kind = ExprKind::FnRel(self.template.fn_rels.push(match *fn_id {
          FnId::Concrete(fn_id) => Rel::Def(fn_id, impls),
          FnId::Abstract(_, fn_id) => Rel::Subitem(impls.pop().unwrap(), fn_id),
        }));
      }
      _ => (),
    }
    self._visit_expr(expr);
  }
}

#[derive(Debug, Default)]
struct TemplateInfo<'core> {
  impl_params: usize,
  fn_rels: IdxVec<FnRelId, Rel<'core, ConcreteFnId, TraitFnId>>,
  const_rels: IdxVec<ConstRelId, Rel<'core, ConcreteConstId, TraitConstId>>,
  specs: HashMap<Vec<ImplTree>, SpecId>,
}

new_idx!(pub FnRelId);
new_idx!(pub ConstRelId);
new_idx!(pub SpecId);

#[derive(Debug)]
pub struct Spec {
  pub def: DefId,
  pub template: TemplateId,
  pub index: usize,
  pub singular: bool,
  pub rels: SpecRels,
}

#[derive(Debug)]
pub struct SpecRels {
  pub fns: IdxVec<FnRelId, Result<SpecId, ErrorGuaranteed>>,
  pub consts: IdxVec<ConstRelId, Result<SpecId, ErrorGuaranteed>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ImplTree(ImplId, Vec<ImplTree>);

#[derive(Debug)]
enum Rel<'core, ConcreteId, AbstractId> {
  Def(ConcreteId, Vec<Impl<'core>>),
  Subitem(Impl<'core>, AbstractId),
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
    self.consts.truncate(checkpoint.concrete_consts.0);
    self.fns.truncate(checkpoint.concrete_fns.0);
    self.consts.values_mut().for_each(|info| info.specs.retain(|_, s| *s < specs));
    self.fns.values_mut().for_each(|info| info.specs.retain(|_, s| *s < specs));
  }
}
