use std::collections::hash_map::Entry;

use ivy::ast::Nets;
use vine_util::idx::IdxVec;

use crate::{
  components::emitter::Emitter,
  structures::{
    ast::Ident,
    chart::{Chart, ConstId, FnId, TraitFnId},
    checkpoint::Checkpoint,
    core::Core,
    diag::ErrorGuaranteed,
    resolutions::{
      Become, FnRel, Fragment, FragmentId, Resolutions, ResolvedImpl, ResolvedImplKind,
    },
    specializations::{ImplTree, Spec, SpecId, SpecRels, Specializations},
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
    let rels = SpecRels {
      fns: IdxVec::from_iter(
        rels.fns.values().map(|rel| self.instantiate_fn_rel(id, &impl_args, rel)),
      ),
      consts: IdxVec::from_iter(
        rels.consts.values().map(|rel| self.instantiate_const_rel(id, &impl_args, rel)),
      ),
    };
    let path = self.fragments[id].path;
    let spec = Spec { fragment: Some(id), path, index, singular: impl_args.is_empty(), rels };
    self.specs.specs[spec_id] = Some(spec);
    spec_id
  }

  fn instantiate_const_rel(
    &mut self,
    fragment_id: FragmentId,
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
            | ImplTree::DropClosure(..)
            | ImplTree::Tuple(..) => unreachable!(),
            ImplTree::Def(impl_id, mut inner_impls) => {
              match &self.resolutions.impls[impl_id].as_ref()?.kind {
                ResolvedImplKind::Direct { consts, .. } => {
                  inner_impls.append(&mut impls);
                  Ok(self.specialize(self.resolutions.consts[consts[const_id]?], impls))
                }
                ResolvedImplKind::Indirect(next_impl) => {
                  impl_ = self.instantiate(fragment_id, args, next_impl);
                  continue;
                }
              }
            }
            ImplTree::Object(ident, _) => Ok(self.ident_const(ident)),
            ImplTree::Struct(ident) => Ok(self.ident_const(ident)),
          };
        }
      }
    }
  }

  fn instantiate_fn_rel(
    &mut self,
    fragment_id: FragmentId,
    args: &Vec<ImplTree<'core>>,
    fn_rel: &FnRel<'core>,
  ) -> Result<(SpecId, StageId), ErrorGuaranteed> {
    match fn_rel {
      &FnRel::Item(fn_id, ref impls) => {
        let impls = impls.iter().map(|x| self.instantiate(fragment_id, args, x)).collect();
        self.instantiate_fn_id(fragment_id, args, fn_id, impls)
      }
      FnRel::Impl(impl_) => {
        let impl_ = self.instantiate(fragment_id, args, impl_);
        match impl_ {
          ImplTree::Error(err) => Err(err),
          ImplTree::Def(..)
          | ImplTree::ForkClosure(..)
          | ImplTree::DropClosure(..)
          | ImplTree::Tuple(..)
          | ImplTree::Object(..)
          | ImplTree::Struct(..) => {
            unreachable!()
          }
          ImplTree::Fn(fn_id, impls) => self.instantiate_fn_id(fragment_id, args, fn_id, impls),
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
    fragment_id: FragmentId,
    args: &Vec<ImplTree<'core>>,
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
                  impl_ = self.instantiate(fragment_id, args, next_impl);
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
            ImplTree::Tuple(len) | ImplTree::Object(_, len) => match fn_id {
              TraitFnId(0) => Ok((self.composite_deconstruct(len), StageId(0))),
              TraitFnId(1) => Ok((self.composite_reconstruct(len), StageId(0))),
              _ => unreachable!(),
            },
            ImplTree::Struct(_) => Ok((self.identity(), StageId(0))),
            ImplTree::Error(err) => Err(err),
            ImplTree::Fn(..) | ImplTree::Closure(..) => unreachable!(),
          };
        }
      }
    }
  }

  fn composite_deconstruct(&mut self, len: usize) -> SpecId {
    *self.specs.composite_deconstruct.entry(len).or_insert_with(|| {
      let path_owned = format!("::auto:composite_deconstruct:{len}");
      let path = self.core.alloc_str(&path_owned);
      self.nets.insert(path_owned, Emitter::composite_deconstruct(len));
      self.specs.specs.push(Some(Spec::synthetic(path)))
    })
  }

  fn composite_reconstruct(&mut self, len: usize) -> SpecId {
    *self.specs.composite_reconstruct.entry(len).or_insert_with(|| {
      let path_owned = format!("::auto:composite_reconstruct:{len}");
      let path = self.core.alloc_str(&path_owned);
      self.nets.insert(path_owned, Emitter::composite_reconstruct(len));
      self.specs.specs.push(Some(Spec::synthetic(path)))
    })
  }

  fn ident_const(&mut self, key: Ident<'core>) -> SpecId {
    *self.specs.ident_const.entry(key).or_insert_with(|| {
      let path_owned = format!("::auto:ident_const:{}", key.0 .0);
      let path = self.core.alloc_str(&path_owned);
      self.nets.insert(path_owned, Emitter::ident_const(key));
      self.specs.specs.push(Some(Spec::synthetic(path)))
    })
  }

  fn identity(&mut self) -> SpecId {
    *self.specs.identity.get_or_insert_with(|| {
      let path_owned = "::auto:identity".to_owned();
      let path = self.core.alloc_str(&path_owned);
      self.nets.insert(path_owned, Emitter::identity());
      self.specs.specs.push(Some(Spec::synthetic(path)))
    })
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
    fragment_id: FragmentId,
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
      TirImpl::Closure(id) => ImplTree::Closure(fragment_id, args.clone(), *id),
      TirImpl::ForkClosure(id) => ImplTree::ForkClosure(fragment_id, args.clone(), *id),
      TirImpl::DropClosure(id) => ImplTree::DropClosure(fragment_id, args.clone(), *id),
      TirImpl::Tuple(len) => ImplTree::Tuple(*len),
      TirImpl::Object(key, len) => ImplTree::Object(*key, *len),
      TirImpl::Struct(name) => ImplTree::Struct(*name),
    }
  }
}
