use std::collections::hash_map::Entry;

use ivy::ast::Nets;
use vine_util::idx::IdxVec;

use crate::{
  components::synthesizer::SyntheticItem,
  structures::{
    chart::{Chart, ConstId, FnId},
    checkpoint::Checkpoint,
    core::Core,
    diag::ErrorGuaranteed,
    resolutions::{
      Become, FnRel, Fragment, FragmentId, Rels, Resolutions, ResolvedImpl, ResolvedImplKind,
    },
    specializations::{ImplTree, Spec, SpecId, SpecKind, SpecRels, Specializations},
    tir::{ClosureId, TirImpl},
    vir::{InterfaceKind, StageId, Vir},
  },
};

#[derive(Debug)]
pub struct Specializer<'core, 'a> {
  pub core: &'core Core<'core>,
  pub chart: &'a Chart<'core>,
  pub resolutions: &'a Resolutions<'core>,
  pub specs: &'a mut Specializations<'core>,
  pub fragments: &'a IdxVec<FragmentId, Fragment<'core>>,
  pub vir: &'a IdxVec<FragmentId, Vir<'core>>,
  pub nets: &'a mut Nets,
}

impl<'core, 'a> Specializer<'core, 'a> {
  pub fn specialize_since(&mut self, checkpoint: &Checkpoint) {
    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      if self.fragments[fragment_id].impl_params == 0 {
        self.specialize(fragment_id, Vec::new());
      }
    }
  }

  fn specialize(&mut self, id: FragmentId, impl_args: Vec<ImplTree<'core>>) -> SpecId {
    let spec_id = self.specs.specs.next_index();
    let lookup = self.specs.lookup.get_or_extend(id);
    let index = lookup.len();
    let entry = match lookup.entry(impl_args) {
      Entry::Occupied(e) => return *e.get(),
      Entry::Vacant(e) => e,
    };
    let impl_args = entry.key().clone();
    entry.insert(spec_id);
    self.specs.specs.push_to(spec_id, None);
    let rels = &self.vir[id].rels;
    let rels = self.instantiate_rels(Some(id), &impl_args, rels);
    let path = self.fragments[id].path;
    let spec =
      Spec { path, index, singular: impl_args.is_empty(), rels, kind: SpecKind::Fragment(id) };
    self.specs.specs[spec_id] = Some(spec);
    spec_id
  }

  fn instantiate_rels(
    &mut self,
    fragment_id: Option<FragmentId>,
    args: &Vec<ImplTree<'core>>,
    rels: &Rels<'core>,
  ) -> SpecRels {
    let rels = SpecRels {
      fns: IdxVec::from_iter(
        rels.fns.values().map(|rel| self.instantiate_fn_rel(fragment_id, args, rel)),
      ),
      consts: IdxVec::from_iter(
        rels.consts.values().map(|rel| self.instantiate_const_rel(fragment_id, args, rel)),
      ),
    };
    rels
  }

  fn instantiate_const_rel(
    &mut self,
    fragment_id: Option<FragmentId>,
    args: &Vec<ImplTree<'core>>,
    (const_id, impls): &(ConstId, Vec<TirImpl<'core>>),
  ) -> Result<SpecId, ErrorGuaranteed> {
    let mut impls = impls.iter().map(|x| self.instantiate(fragment_id, args, x)).collect();
    match *const_id {
      ConstId::Concrete(const_id) => Ok(self.specialize(self.resolutions.consts[const_id], impls)),
      ConstId::Abstract(_, const_id) => {
        let mut impl_ = impls.remove(0);
        loop {
          break match impl_ {
            ImplTree::Error(err) => Err(err),
            ImplTree::Fn(..)
            | ImplTree::Closure(..)
            | ImplTree::ForkClosure(..)
            | ImplTree::DropClosure(..) => unreachable!(),
            ImplTree::Def(impl_id, mut inner_impls) => {
              match &self.resolutions.impls[impl_id].as_ref()?.kind {
                ResolvedImplKind::Direct { consts, .. } => {
                  inner_impls.append(&mut impls);
                  Ok(self.specialize(self.resolutions.consts[consts[const_id]?], impls))
                }
                ResolvedImplKind::Indirect(next_impl) => {
                  impl_ = self.instantiate(None, &inner_impls, next_impl);
                  continue;
                }
              }
            }
            ImplTree::Synthetic(impl_) => {
              let item = impl_.const_(self.chart, const_id);
              Ok(self.instantiate_synthetic_item(item, impls))
            }
          };
        }
      }
    }
  }

  fn instantiate_fn_rel(
    &mut self,
    fragment_id: Option<FragmentId>,
    args: &Vec<ImplTree<'core>>,
    fn_rel: &FnRel<'core>,
  ) -> Result<(SpecId, StageId), ErrorGuaranteed> {
    match fn_rel {
      &FnRel::Item(fn_id, ref impls) => {
        let impls = impls.iter().map(|x| self.instantiate(fragment_id, args, x)).collect();
        self.instantiate_fn_id(fn_id, impls)
      }
      FnRel::Impl(impl_) => {
        let impl_ = self.instantiate(fragment_id, args, impl_);
        match impl_ {
          ImplTree::Error(err) => Err(err),
          ImplTree::Def(..)
          | ImplTree::ForkClosure(..)
          | ImplTree::DropClosure(..)
          | ImplTree::Synthetic(..) => {
            unreachable!()
          }
          ImplTree::Fn(fn_id, impls) => self.instantiate_fn_id(fn_id, impls),
          ImplTree::Closure(fragment_id, impls, closure_id) => Ok((
            self.specialize(fragment_id, impls),
            self._closure_stage(fragment_id, Some(closure_id)),
          )),
        }
      }
    }
  }

  fn instantiate_fn_id(
    &mut self,
    fn_id: FnId,
    mut impls: Vec<ImplTree<'core>>,
  ) -> Result<(SpecId, StageId), ErrorGuaranteed> {
    match fn_id {
      FnId::Concrete(fn_id) => {
        let fragment_id = self.resolutions.fns[fn_id];
        Ok((self.specialize(fragment_id, impls), self._closure_stage(fragment_id, None)))
      }
      FnId::Abstract(_, fn_id) => {
        let mut impl_ = impls.remove(0);
        loop {
          break match impl_ {
            ImplTree::Def(impl_id, mut inner_impls) => {
              match &self.resolutions.impls[impl_id].as_ref()?.kind {
                ResolvedImplKind::Direct { fns, .. } => {
                  inner_impls.append(&mut impls);
                  let fragment_id = self.resolutions.fns[fns[fn_id]?];
                  Ok((
                    self.specialize(fragment_id, inner_impls),
                    self._closure_stage(fragment_id, None),
                  ))
                }
                ResolvedImplKind::Indirect(next_impl) => {
                  impl_ = self.instantiate(None, &inner_impls, next_impl);
                  continue;
                }
              }
            }
            ImplTree::ForkClosure(fragment_id, impls, closure_id) => {
              match self._closure_interface(fragment_id, Some(closure_id)) {
                &InterfaceKind::Fn { fork: Some(stage), .. } => {
                  Ok((self.specialize(fragment_id, impls), stage))
                }
                _ => unreachable!(),
              }
            }
            ImplTree::DropClosure(fragment_id, impls, closure_id) => {
              match self._closure_interface(fragment_id, Some(closure_id)) {
                &InterfaceKind::Fn { drop: Some(stage), .. } => {
                  Ok((self.specialize(fragment_id, impls), stage))
                }
                _ => unreachable!(),
              }
            }
            ImplTree::Error(err) => Err(err),
            ImplTree::Fn(..) | ImplTree::Closure(..) => unreachable!(),
            ImplTree::Synthetic(impl_) => {
              let item = impl_.fn_(self.chart, fn_id);
              Ok((self.instantiate_synthetic_item(item, impls), StageId(0)))
            }
          };
        }
      }
    }
  }

  fn instantiate_synthetic_item(
    &mut self,
    item: SyntheticItem<'core>,
    impls: Vec<ImplTree<'core>>,
  ) -> SpecId {
    let index = self.specs.synthetic.len();
    match self.specs.synthetic.entry((item, impls)) {
      Entry::Occupied(e) => *e.get(),
      Entry::Vacant(entry) => {
        let (_, impls) = entry.key().clone();
        let spec_id = self.specs.specs.push(None);
        entry.insert(spec_id);
        let rels = self.instantiate_rels(None, &impls, &item.rels());
        self.specs.specs[spec_id] = Some(Spec {
          path: ":synthetic",
          index,
          singular: false,
          rels,
          kind: SpecKind::Synthetic(item),
        });
        spec_id
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
      None => vir.closures[ClosureId(0)],
    };
    &vir.interfaces[interface_id].kind
  }

  fn instantiate(
    &self,
    fragment_id: Option<FragmentId>,
    args: &Vec<ImplTree<'core>>,
    impl_: &TirImpl<'core>,
  ) -> ImplTree<'core> {
    match impl_ {
      TirImpl::Error(err) => ImplTree::Error(*err),
      TirImpl::Param(i) => args[*i].clone(),
      TirImpl::Def(id, impls) => {
        let impls =
          impls.iter().map(|i| self.instantiate(fragment_id, args, i)).collect::<Vec<_>>();
        if let Ok(ResolvedImpl {
          become_: Become::Resolved(Some((become_impl, indices))), ..
        }) = &self.resolutions.impls[*id]
        {
          if indices.iter().all(|&i| matches!(impls[i], ImplTree::Def(id, _) if id == *become_impl))
          {
            return ImplTree::Def(*become_impl, vec![]);
          }
        }
        ImplTree::Def(*id, impls)
      }
      TirImpl::Fn(id, impls) => {
        ImplTree::Fn(*id, impls.iter().map(|i| self.instantiate(fragment_id, args, i)).collect())
      }
      TirImpl::Closure(id) => ImplTree::Closure(fragment_id.unwrap(), args.clone(), *id),
      TirImpl::ForkClosure(id) => ImplTree::ForkClosure(fragment_id.unwrap(), args.clone(), *id),
      TirImpl::DropClosure(id) => ImplTree::DropClosure(fragment_id.unwrap(), args.clone(), *id),
      TirImpl::Synthetic(synthetic) => ImplTree::Synthetic(*synthetic),
    }
  }
}
