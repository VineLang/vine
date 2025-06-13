use std::collections::{hash_map::Entry, HashMap};

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  components::resolver::Resolutions,
  structures::{
    chart::{Chart, ConcreteConstId, ConcreteFnId, ConstId, DefId, FnId, GenericsId, ImplId},
    checkpoint::Checkpoint,
    diag::ErrorGuaranteed,
    tir::{ConstRelId, FnRelId, Tir, TirImpl},
  },
};

#[derive(Debug)]
pub struct Specializer<'core, 'a> {
  pub chart: &'a Chart<'core>,
  pub resolutions: &'a Resolutions,
  pub specs: &'a mut Specializations,
}

#[derive(Debug, Default)]
pub struct Specializations {
  pub consts: IdxVec<ConcreteConstId, TemplateInfo>,
  pub fns: IdxVec<ConcreteFnId, TemplateInfo>,
  pub specs: IdxVec<SpecId, Option<Spec>>,
}

impl<'core, 'a> Specializer<'core, 'a> {
  pub fn specialize_since(&mut self, checkpoint: &Checkpoint) {
    self.initialize(checkpoint);
    self.specialize_roots(checkpoint);
  }

  fn initialize(&mut self, checkpoint: &Checkpoint) {
    for const_id in self.chart.concrete_consts.keys_from(checkpoint.concrete_consts) {
      self.specs.consts.push(TemplateInfo {
        impl_params: self.impl_param_count(self.chart.concrete_consts[const_id].generics),
        const_rels: self.resolutions.consts[const_id].const_rels.clone(),
        fn_rels: self.resolutions.consts[const_id].fn_rels.clone(),
        specs: HashMap::new(),
      });
    }

    for fn_id in self.chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      self.specs.fns.push(TemplateInfo {
        impl_params: self.impl_param_count(self.chart.concrete_fns[fn_id].generics),
        const_rels: self.resolutions.fns[fn_id].const_rels.clone(),
        fn_rels: self.resolutions.fns[fn_id].fn_rels.clone(),
        specs: HashMap::new(),
      });
    }
  }

  fn specialize_roots(&mut self, checkpoint: &Checkpoint) {
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

  fn instantiate_const_rel(
    &self,
    (const_id, impls): &(ConstId, Vec<TirImpl>),
    impl_args: &[ImplTree],
  ) -> Result<(ConcreteConstId, Vec<ImplTree>), ErrorGuaranteed> {
    match *const_id {
      ConstId::Concrete(const_id) => {
        Ok((const_id, impls.iter().map(|x| instantiate(x, impl_args)).collect::<Result<_, _>>()?))
      }
      ConstId::Abstract(_, trait_const_id) => {
        let [impl_] = &**impls else { unreachable!() };
        let ImplTree(impl_id, impls) = instantiate(impl_, impl_args)?;
        Ok((self.resolutions.impls[impl_id].as_ref()?.consts[trait_const_id]?, impls))
      }
    }
  }

  fn instantiate_fn_rel(
    &self,
    (fn_id, impls): &(FnId, Vec<TirImpl>),
    impl_args: &[ImplTree],
  ) -> Result<(ConcreteFnId, Vec<ImplTree>), ErrorGuaranteed> {
    match *fn_id {
      FnId::Concrete(fn_id) => {
        Ok((fn_id, impls.iter().map(|x| instantiate(x, impl_args)).collect::<Result<_, _>>()?))
      }
      FnId::Abstract(_, fn_id) => {
        let [impl_] = &**impls else { unreachable!() };
        let ImplTree(impl_id, impls) = instantiate(impl_, impl_args)?;
        Ok((self.resolutions.impls[impl_id].as_ref()?.fns[fn_id]?, impls))
      }
    }
  }

  pub(crate) fn _specialize_custom(&mut self, tir: &Tir) -> SpecRels {
    SpecRels {
      fns: IdxVec::from(Vec::from_iter(tir.fn_rels.values().map(|rel| {
        let (fn_id, args) = self.instantiate_fn_rel(rel, &[])?;
        Ok(self.specialize(TemplateId::Fn(fn_id), args))
      }))),
      consts: IdxVec::from(Vec::from_iter(tir.const_rels.values().map(|rel| {
        let (const_id, args) = self.instantiate_const_rel(rel, &[])?;
        Ok(self.specialize(TemplateId::Const(const_id), args))
      }))),
    }
  }

  fn template(&self, id: TemplateId) -> &TemplateInfo {
    match id {
      TemplateId::Const(const_id) => &self.specs.consts[const_id],
      TemplateId::Fn(fn_id) => &self.specs.fns[fn_id],
    }
  }

  fn template_mut(&mut self, id: TemplateId) -> &mut TemplateInfo {
    match id {
      TemplateId::Const(const_id) => &mut self.specs.consts[const_id],
      TemplateId::Fn(fn_id) => &mut self.specs.fns[fn_id],
    }
  }

  fn impl_param_count(&self, generics_id: GenericsId) -> usize {
    let generics = &self.chart.generics[generics_id];
    generics
      .type_params
      .iter()
      .map(|p| p.flex.fork() as usize + p.flex.drop() as usize)
      .sum::<usize>()
      + generics.impl_params.len()
  }
}

#[derive(Debug, Clone, Copy)]
pub enum TemplateId {
  Const(ConcreteConstId),
  Fn(ConcreteFnId),
}

#[derive(Debug, Default)]
pub struct TemplateInfo {
  impl_params: usize,
  const_rels: IdxVec<ConstRelId, (ConstId, Vec<TirImpl>)>,
  fn_rels: IdxVec<FnRelId, (FnId, Vec<TirImpl>)>,
  pub specs: HashMap<Vec<ImplTree>, SpecId>,
}

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
pub struct ImplTree(ImplId, Vec<ImplTree>);

fn instantiate(impl_: &TirImpl, params: &[ImplTree]) -> Result<ImplTree, ErrorGuaranteed> {
  match &impl_ {
    TirImpl::Error(err) => Err(*err),
    TirImpl::Param(i) => Ok(params[*i].clone()),
    TirImpl::Def(id, impls) => {
      Ok(ImplTree(*id, impls.iter().map(|i| instantiate(i, params)).collect::<Result<_, _>>()?))
    }
  }
}
