use std::collections::hash_map::Entry;

use vine_util::idx::IdxVec;

use crate::structures::{
  chart::{Chart, ConstId, FnId},
  checkpoint::Checkpoint,
  diag::ErrorGuaranteed,
  resolutions::{Fragment, FragmentId, Resolutions},
  specializations::{ImplTree, Spec, SpecId, SpecRels, Specializations},
  tir::TirImpl,
};

#[derive(Debug)]
pub struct Specializer<'core, 'a> {
  pub chart: &'a Chart<'core>,
  pub resolutions: &'a Resolutions,
  pub specs: &'a mut Specializations,
  pub fragments: &'a IdxVec<FragmentId, Fragment<'core>>,
}

impl<'core, 'a> Specializer<'core, 'a> {
  pub fn specialize_since(&mut self, checkpoint: &Checkpoint) {
    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      if self.fragments[fragment_id].impl_params == 0 {
        self.specialize(fragment_id, Vec::new());
      }
    }
  }

  fn specialize(&mut self, id: FragmentId, impl_args: Vec<ImplTree>) -> SpecId {
    let spec_id = self.specs.specs.next_index();
    let lookup = self.specs.lookup.get_or_extend(id);
    let index = lookup.len();
    let entry = match lookup.entry(impl_args) {
      Entry::Occupied(e) => return *e.get(),
      Entry::Vacant(e) => e,
    };
    let impl_args = entry.key().clone();
    entry.insert(spec_id);
    self.specs.specs.push(None);
    let fragment = &self.fragments[id];
    let rels = SpecRels {
      fns: IdxVec::from(Vec::from_iter(fragment.fn_rels.values().map(|rel| {
        let (id, args) = self.instantiate_fn_rel(rel, &impl_args)?;
        Ok(self.specialize(id, args))
      }))),
      consts: IdxVec::from(Vec::from_iter(fragment.const_rels.values().map(|rel| {
        let (id, args) = self.instantiate_const_rel(rel, &impl_args)?;
        Ok(self.specialize(id, args))
      }))),
    };
    let spec = Spec { fragment: id, index, singular: impl_args.is_empty(), rels };
    self.specs.specs[spec_id] = Some(spec);
    spec_id
  }

  fn instantiate_const_rel(
    &self,
    (const_id, impls): &(ConstId, Vec<TirImpl>),
    impl_args: &[ImplTree],
  ) -> Result<(FragmentId, Vec<ImplTree>), ErrorGuaranteed> {
    match *const_id {
      ConstId::Concrete(const_id) => {
        let impls = impls.iter().map(|x| instantiate(x, impl_args)).collect::<Result<_, _>>()?;
        Ok((self.resolutions.consts[const_id], impls))
      }
      ConstId::Abstract(_, const_id) => {
        let [impl_] = &**impls else { unreachable!() };
        let ImplTree(impl_id, impls) = instantiate(impl_, impl_args)?;
        let const_id = self.resolutions.impls[impl_id].as_ref()?.consts[const_id]?;
        Ok((self.resolutions.consts[const_id], impls))
      }
    }
  }

  fn instantiate_fn_rel(
    &self,
    (fn_id, impls): &(FnId, Vec<TirImpl>),
    impl_args: &[ImplTree],
  ) -> Result<(FragmentId, Vec<ImplTree>), ErrorGuaranteed> {
    match *fn_id {
      FnId::Concrete(fn_id) => {
        let impls = impls.iter().map(|x| instantiate(x, impl_args)).collect::<Result<_, _>>()?;
        Ok((self.resolutions.fns[fn_id], impls))
      }
      FnId::Abstract(_, fn_id) => {
        let [impl_] = &**impls else { unreachable!() };
        let ImplTree(impl_id, impls) = instantiate(impl_, impl_args)?;
        let fn_id = self.resolutions.impls[impl_id].as_ref()?.fns[fn_id]?;
        Ok((self.resolutions.fns[fn_id], impls))
      }
    }
  }
}

fn instantiate(impl_: &TirImpl, params: &[ImplTree]) -> Result<ImplTree, ErrorGuaranteed> {
  match &impl_ {
    TirImpl::Error(err) => Err(*err),
    TirImpl::Param(i) => Ok(params[*i].clone()),
    TirImpl::Def(id, impls) => {
      Ok(ImplTree(*id, impls.iter().map(|i| instantiate(i, params)).collect::<Result<_, _>>()?))
    }
  }
}
