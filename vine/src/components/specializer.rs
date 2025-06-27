use std::collections::hash_map::Entry;

use vine_util::idx::IdxVec;

use crate::structures::{
  chart::{Chart, ConstId, FnId, ImplId},
  checkpoint::Checkpoint,
  diag::ErrorGuaranteed,
  resolutions::{FnRel, Fragment, FragmentId, Resolutions},
  specializations::{ImplTree, Spec, SpecId, SpecRels, Specializations},
  tir::{ClosureId, TirImpl},
  vir::{InterfaceKind, StageId, Vir},
};

#[derive(Debug)]
pub struct Specializer<'core, 'a> {
  pub chart: &'a Chart<'core>,
  pub resolutions: &'a Resolutions,
  pub specs: &'a mut Specializations<'core>,
  pub fragments: &'a IdxVec<FragmentId, Fragment<'core>>,
  pub vir: &'a IdxVec<FragmentId, Vir<'core>>,
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
    let rels = &self.vir[id].rels;
    let rels = SpecRels {
      fns: IdxVec::from(Vec::from_iter(rels.fns.values().map(|rel| {
        let (id, args, stage) = self.instantiate_fn_rel(id, &impl_args, rel)?;
        Ok((self.specialize(id, args), stage))
      }))),
      consts: IdxVec::from(Vec::from_iter(rels.consts.values().map(|rel| {
        let (id, args) = self.instantiate_const_rel(id, &impl_args, rel)?;
        Ok(self.specialize(id, args))
      }))),
    };
    let path = self.fragments[id].path;
    let spec = Spec { fragment: id, path, index, singular: impl_args.is_empty(), rels };
    self.specs.specs[spec_id] = Some(spec);
    spec_id
  }

  fn instantiate_const_rel(
    &self,
    fragment_id: FragmentId,
    args: &Vec<ImplTree>,
    (const_id, impls): &(ConstId, Vec<TirImpl>),
  ) -> Result<(FragmentId, Vec<ImplTree>), ErrorGuaranteed> {
    match *const_id {
      ConstId::Concrete(const_id) => {
        let impls = impls.iter().map(|x| instantiate(fragment_id, args, x)).collect();
        Ok((self.resolutions.consts[const_id], impls))
      }
      ConstId::Abstract(_, const_id) => {
        let [impl_] = &**impls else { unreachable!() };
        let (impl_id, impls) = instantiate(fragment_id, args, impl_).assume_def()?;
        let const_id = self.resolutions.impls[impl_id].as_ref()?.consts[const_id]?;
        Ok((self.resolutions.consts[const_id], impls))
      }
    }
  }

  fn instantiate_fn_rel(
    &self,
    fragment_id: FragmentId,
    args: &Vec<ImplTree>,
    fn_rel: &FnRel,
  ) -> Result<(FragmentId, Vec<ImplTree>, StageId), ErrorGuaranteed> {
    match fn_rel {
      FnRel::Item(fn_id, impls) => {
        let impls = impls.iter().map(|x| instantiate(fragment_id, args, x)).collect();
        self.instantiate_fn_id(*fn_id, impls)
      }
      FnRel::Impl(impl_) => {
        let impl_ = instantiate(fragment_id, args, impl_);
        match impl_ {
          ImplTree::Error(err) => Err(err),
          ImplTree::Def(..) | ImplTree::ForkClosure(..) | ImplTree::DropClosure(..) => {
            unreachable!()
          }
          ImplTree::Fn(fn_id, impls) => self.instantiate_fn_id(fn_id, impls),
          ImplTree::Closure(fragment_id, impls, closure_id) => {
            Ok((fragment_id, impls, self._closure_stage(fragment_id, Some(closure_id))))
          }
        }
      }
    }
  }

  fn instantiate_fn_id(
    &self,
    fn_id: FnId,
    mut impls: Vec<ImplTree>,
  ) -> Result<(FragmentId, Vec<ImplTree>, StageId), ErrorGuaranteed> {
    match fn_id {
      FnId::Concrete(fn_id) => {
        let fragment_id = self.resolutions.fns[fn_id];
        Ok((fragment_id, impls, self._closure_stage(fragment_id, None)))
      }
      FnId::Abstract(_, fn_id) => {
        assert_eq!(impls.len(), 1);
        let impl_ = impls.pop().unwrap();
        match impl_ {
          ImplTree::Def(impl_id, impls) => {
            let fn_id = self.resolutions.impls[impl_id].as_ref()?.fns[fn_id]?;
            let fragment_id = self.resolutions.fns[fn_id];
            Ok((fragment_id, impls, self._closure_stage(fragment_id, None)))
          }
          ImplTree::ForkClosure(fragment_id, impls, closure_id) => {
            match self._closure_interface(fragment_id, Some(closure_id)) {
              InterfaceKind::Fn { fork: Some(stage), .. } => Ok((fragment_id, impls, *stage)),
              _ => unreachable!(),
            }
          }
          ImplTree::DropClosure(fragment_id, impls, closure_id) => {
            match self._closure_interface(fragment_id, Some(closure_id)) {
              InterfaceKind::Fn { drop: Some(stage), .. } => Ok((fragment_id, impls, *stage)),
              _ => unreachable!(),
            }
          }
          ImplTree::Error(err) => Err(err),
          ImplTree::Fn(..) | ImplTree::Closure(..) => unreachable!(),
        }
      }
    }
  }

  fn _closure_stage(&self, fragment_id: FragmentId, closure_id: Option<ClosureId>) -> StageId {
    match self._closure_interface(fragment_id, closure_id) {
      InterfaceKind::Fn { call, .. } => *call,
      _ => unreachable!(),
    }
  }

  fn _closure_interface(
    &self,
    fragment_id: FragmentId,
    closure_id: Option<ClosureId>,
  ) -> &InterfaceKind {
    let vir = &self.vir[fragment_id];
    let interface_id = match closure_id {
      Some(id) => vir.closures[id],
      None => *vir.closures.last().unwrap(),
    };
    &vir.interfaces[interface_id].kind
  }
}

fn instantiate(fragment_id: FragmentId, args: &Vec<ImplTree>, impl_: &TirImpl) -> ImplTree {
  match impl_ {
    TirImpl::Error(err) => ImplTree::Error(*err),
    TirImpl::Param(i) => args[*i].clone(),
    TirImpl::Def(id, impls) => {
      ImplTree::Def(*id, impls.iter().map(|i| instantiate(fragment_id, args, i)).collect())
    }
    TirImpl::Fn(id, impls) => {
      ImplTree::Fn(*id, impls.iter().map(|i| instantiate(fragment_id, args, i)).collect())
    }
    TirImpl::Closure(id) => ImplTree::Closure(fragment_id, args.clone(), *id),
    TirImpl::ForkClosure(id) => ImplTree::ForkClosure(fragment_id, args.clone(), *id),
    TirImpl::DropClosure(id) => ImplTree::DropClosure(fragment_id, args.clone(), *id),
  }
}

impl ImplTree {
  fn assume_def(self) -> Result<(ImplId, Vec<ImplTree>), ErrorGuaranteed> {
    match self {
      ImplTree::Def(impl_id, impls) => Ok((impl_id, impls)),
      ImplTree::Error(err) => Err(err),
      _ => unreachable!(),
    }
  }
}
