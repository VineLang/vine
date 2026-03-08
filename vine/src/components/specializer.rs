use std::collections::hash_map::Entry;

use hedera::name::{Name, NameId, PathId, Table};
use vine_util::{idx::IdxVec, nat::Nat};

use crate::{
  compiler::Guide,
  components::synthesizer::{SyntheticImpl, SyntheticItem},
  features::debug::frame_data,
  structures::{
    chart::{Chart, ConstId, FnId},
    checkpoint::Checkpoint,
    diag::ErrorGuaranteed,
    resolutions::{
      Become, Fragment, FragmentId, Rels, Resolutions, ResolvedImpl, ResolvedImplKind,
    },
    specializations::{
      ImplTree, ImplTreeId, ImplTreeKind, Spec, SpecId, SpecKind, SpecRels, Specializations,
    },
    tir::{ClosureId, TirImpl},
    vir::{InterfaceKind, StageId, Vir},
  },
};

#[derive(Debug)]
pub struct Specializer<'a> {
  pub chart: &'a Chart,
  pub resolutions: &'a Resolutions,
  pub specs: &'a mut Specializations,
  pub fragments: &'a IdxVec<FragmentId, Fragment>,
  pub vir: &'a IdxVec<FragmentId, Vir>,
  pub guide: &'a Guide,
  pub table: &'a mut Table,
}

impl<'a> Specializer<'a> {
  pub fn specialize_since(&mut self, checkpoint: &Checkpoint) {
    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      if self.fragments[fragment_id].impl_params == 0 {
        self.specialize(fragment_id, Vec::new());
      }
    }
  }

  fn specialize(&mut self, id: FragmentId, impl_args: Vec<ImplTreeId>) -> SpecId {
    let spec_id = self.specs.specs.next_index();
    let lookup = self.specs.lookup.get_or_extend(id);
    let entry = match lookup.entry(impl_args) {
      Entry::Occupied(e) => return *e.get(),
      Entry::Vacant(e) => e,
    };
    let impl_args = entry.key().clone();
    entry.insert(spec_id);
    self.specs.specs.push_to(spec_id, None);
    let rels = &self.vir[id].rels;
    let rels = self.instantiate_rels(Some(id), &impl_args, rels);
    let path = add_path(self.table, &self.fragments[id].path);
    let name = path.with_children(impl_args.iter().map(|&i| self.specs.impls[i].name));
    let spec = Spec { name, rels, kind: SpecKind::Fragment(id) };
    self.specs.specs[spec_id] = Some(spec);
    spec_id
  }

  fn instantiate_rels(
    &mut self,
    fragment_id: Option<FragmentId>,
    args: &Vec<ImplTreeId>,
    rels: &Rels,
  ) -> SpecRels {
    SpecRels {
      fns: IdxVec::from_iter(
        rels.fns.values().map(|rel| self.instantiate_fn_rel(fragment_id, args, rel)),
      ),
      consts: IdxVec::from_iter(
        rels.consts.values().map(|rel| self.instantiate_const_rel(fragment_id, args, rel)),
      ),
    }
  }

  fn instantiate_const_rel(
    &mut self,
    fragment_id: Option<FragmentId>,
    args: &Vec<ImplTreeId>,
    (const_id, impls): &(ConstId, Vec<TirImpl>),
  ) -> Result<SpecId, ErrorGuaranteed> {
    let mut impls = impls.iter().map(|x| self.instantiate(fragment_id, args, x)).collect();
    match *const_id {
      ConstId::Concrete(const_id) => {
        if let Some(item) = self.resolutions.config.get(&const_id) {
          Ok(self.instantiate_synthetic_item(item.clone(), Vec::new()))
        } else {
          Ok(self.specialize(self.resolutions.consts[const_id], impls))
        }
      }
      ConstId::Abstract(_, const_id) => {
        let mut impl_ = impls.remove(0);
        loop {
          break match &self.specs.impls[impl_].kind {
            ImplTreeKind::Error(err) => Err(*err),
            ImplTreeKind::Fn(..)
            | ImplTreeKind::Closure(..)
            | ImplTreeKind::ForkClosure(..)
            | ImplTreeKind::DropClosure(..) => unreachable!(),
            ImplTreeKind::Def(impl_id, inner_impls) => {
              let mut inner_impls = inner_impls.clone();
              match &self.resolutions.impls[*impl_id].as_ref()?.kind {
                ResolvedImplKind::Direct { consts, .. } => {
                  inner_impls.append(&mut impls);
                  Ok(self.specialize(self.resolutions.consts[consts[const_id]?], inner_impls))
                }
                ResolvedImplKind::Indirect(next_impl) => {
                  impl_ = self.instantiate(None, &inner_impls, next_impl);
                  continue;
                }
              }
            }
            ImplTreeKind::Synthetic(impl_) => {
              let item = impl_.const_(self.chart, const_id);
              Ok(self.instantiate_synthetic_item(item, impls))
            }
          };
        }
      }
    }
  }

  pub(crate) fn instantiate_fn_rel(
    &mut self,
    fragment_id: Option<FragmentId>,
    args: &Vec<ImplTreeId>,
    (fn_id, impls): &(FnId, Vec<TirImpl>),
  ) -> Result<(SpecId, StageId), ErrorGuaranteed> {
    if self.chart.builtins.debug_state == Some(*fn_id) {
      return Ok((self.instantiate_synthetic_item(SyntheticItem::DebugState, vec![]), StageId(0)));
    }
    let impls = impls.iter().map(|x| self.instantiate(fragment_id, args, x)).collect();
    self._instantiate_fn_rel(*fn_id, impls)
  }

  fn _instantiate_fn_rel(
    &mut self,
    fn_id: FnId,
    mut impls: Vec<ImplTreeId>,
  ) -> Result<(SpecId, StageId), ErrorGuaranteed> {
    match fn_id {
      FnId::Concrete(fn_id) => {
        let fragment_id = self.resolutions.fns[fn_id];
        Ok((self.specialize(fragment_id, impls), StageId(0)))
      }
      FnId::Abstract(_, fn_id) => {
        let mut impl_ = impls.remove(0);
        loop {
          break match &self.specs.impls[impl_].kind {
            ImplTreeKind::Error(err) => Err(*err),
            ImplTreeKind::Def(impl_id, inner_impls) => {
              let mut inner_impls = inner_impls.clone();
              match &self.resolutions.impls[*impl_id].as_ref()?.kind {
                ResolvedImplKind::Direct { fns, .. } => {
                  inner_impls.append(&mut impls);
                  let fragment_id = self.resolutions.fns[fns[fn_id]?];
                  Ok((self.specialize(fragment_id, inner_impls), StageId(0)))
                }
                ResolvedImplKind::Indirect(next_impl) => {
                  impl_ = self.instantiate(None, &inner_impls, next_impl);
                  continue;
                }
              }
            }
            ImplTreeKind::Fn(fn_id, impls, params) => Ok((
              self.instantiate_synthetic_item(
                SyntheticItem::CallFn(*fn_id, impls.len(), *params),
                impls.clone(),
              ),
              StageId(0),
            )),
            &ImplTreeKind::Closure(fragment_id, ref impls, closure_id) => Ok((
              self.specialize(fragment_id, impls.clone()),
              self._closure_stages(fragment_id, closure_id).0,
            )),
            &ImplTreeKind::ForkClosure(fragment_id, ref impls, closure_id) => Ok((
              self.specialize(fragment_id, impls.clone()),
              self._closure_stages(fragment_id, closure_id).1.unwrap(),
            )),
            &ImplTreeKind::DropClosure(fragment_id, ref impls, closure_id) => Ok((
              self.specialize(fragment_id, impls.clone()),
              self._closure_stages(fragment_id, closure_id).2.unwrap(),
            )),
            ImplTreeKind::Synthetic(impl_) => {
              let item = impl_.fn_(self.chart, fn_id);
              Ok((self.instantiate_synthetic_item(item, impls), StageId(0)))
            }
          };
        }
      }
    }
  }

  fn instantiate_synthetic_item(&mut self, item: SyntheticItem, impls: Vec<ImplTreeId>) -> SpecId {
    match self.specs.synthetic.entry((item, impls)) {
      Entry::Occupied(e) => *e.get(),
      Entry::Vacant(entry) => {
        let (item, impls) = entry.key().clone();
        let spec_id = self.specs.specs.push(None);
        entry.insert(spec_id);
        let rels = self.instantiate_rels(None, &impls, &item.rels());
        let mut name = synthetic_item_name(self.chart, self.guide, self.table, &item);
        for impl_ in impls {
          name.children.push(self.specs.impls[impl_].name);
        }
        self.specs.specs[spec_id] = Some(Spec { name, rels, kind: SpecKind::Synthetic(item) });
        spec_id
      }
    }
  }

  fn _closure_stages(
    &self,
    fragment_id: FragmentId,
    closure_id: ClosureId,
  ) -> (StageId, Option<StageId>, Option<StageId>) {
    let vir = &self.vir[fragment_id];
    let interface_id = vir.closures[closure_id];
    let InterfaceKind::Closure { call, fork, drop } = vir.interfaces[interface_id].kind else {
      unreachable!()
    };
    (call, fork, drop)
  }

  pub(crate) fn instantiate(
    &mut self,
    fragment_id: Option<FragmentId>,
    args: &Vec<ImplTreeId>,
    impl_: &TirImpl,
  ) -> ImplTreeId {
    match impl_ {
      TirImpl::Param(i) => args[*i],
      TirImpl::Error(err) => self.add_impl_tree(ImplTreeKind::Error(*err)),
      TirImpl::Def(id, impls) => {
        let impls =
          impls.iter().map(|i| self.instantiate(fragment_id, args, i)).collect::<Vec<_>>();
        if let Ok(ResolvedImpl { become_: Become::Resolved(Some((become_impl, indices))), .. }) =
          &self.resolutions.impls[*id]
          && indices
            .iter()
            .all(|&i| matches!(self.specs.impls[impls[i]].kind, ImplTreeKind::Def(id, _) if id == *become_impl))
        {
           self.add_impl_tree(ImplTreeKind::Def(*become_impl, vec![]))
        } else {
        self.add_impl_tree(ImplTreeKind::Def(*id, impls))
        }
      }
      TirImpl::Fn(id, impls, params) => {
        let impls = impls.iter().map(|i| self.instantiate(fragment_id, args, i)).collect();
        self.add_impl_tree(ImplTreeKind::Fn(*id, impls, *params))
      }
      TirImpl::Closure(id) => {
        self.add_impl_tree(ImplTreeKind::Closure(fragment_id.unwrap(), args.clone(), *id))
      }
      TirImpl::ForkClosure(id) => {
        self.add_impl_tree(ImplTreeKind::ForkClosure(fragment_id.unwrap(), args.clone(), *id))
      }
      TirImpl::DropClosure(id) => {
        self.add_impl_tree(ImplTreeKind::DropClosure(fragment_id.unwrap(), args.clone(), *id))
      }
      TirImpl::Synthetic(synthetic) => {
        self.add_impl_tree(ImplTreeKind::Synthetic(synthetic.clone()))
      }
    }
  }

  fn add_impl_tree(&mut self, kind: ImplTreeKind) -> ImplTreeId {
    *self.specs.impl_lookup.entry(kind).or_insert_with_key(|kind| {
      let name =
        impl_tree_name(self.chart, self.guide, self.table, &self.specs.impls, self.fragments, kind);
      self.specs.impls.push(ImplTree { name: self.table.add_name(name), kind: kind.clone() })
    })
  }
}

fn impl_tree_name(
  chart: &Chart,
  guide: &Guide,
  table: &mut Table,
  impls: &IdxVec<ImplTreeId, ImplTree>,
  fragments: &IdxVec<FragmentId, Fragment>,
  tree: &ImplTreeKind,
) -> Name {
  match tree {
    ImplTreeKind::Error(_) => guide.error.to_name(),
    ImplTreeKind::Def(impl_id, args) => {
      let path = add_path(table, &chart.defs[chart.impls[*impl_id].def].path);
      path.with_children(args.iter().map(|&i| impls[i].name))
    }
    ImplTreeKind::Fn(fn_id, args, _) => {
      let path = add_path(table, &chart.defs[chart.fn_def(*fn_id)].path);
      let name = table.add_name(path.with_children(args.iter().map(|&i| impls[i].name)));
      guide.fn_.with_children([name])
    }
    ImplTreeKind::Closure(fragment_id, args, closure_id) => {
      let path = add_path(table, &fragments[*fragment_id].path);
      let name = table.add_name(path.with_children(args.iter().map(|&i| impls[i].name)));
      guide.closure.with_children([name]).with_data(closure_id.0)
    }
    ImplTreeKind::ForkClosure(fragment_id, args, closure_id) => {
      let path = add_path(table, &fragments[*fragment_id].path);
      let name = table.add_name(path.with_children(args.iter().map(|&i| impls[i].name)));
      guide.closure_fork.with_children([name]).with_data(closure_id.0)
    }
    ImplTreeKind::DropClosure(fragment_id, args, closure_id) => {
      let path = add_path(table, &fragments[*fragment_id].path);
      let name = table.add_name(path.with_children(args.iter().map(|&i| impls[i].name)));
      guide.closure_drop.with_children([name]).with_data(closure_id.0)
    }
    ImplTreeKind::Synthetic(item) => synthetic_impl_name(chart, guide, table, item),
  }
}

fn synthetic_impl_name(
  chart: &Chart,
  guide: &Guide,
  table: &mut Table,
  item: &SyntheticImpl,
) -> Name {
  match item {
    SyntheticImpl::Tuple(len) => guide.synthetic_tuple.with_data(*len),
    SyntheticImpl::Object(ident, len) => {
      let ident = table.add_path(format!(":{ident}"));
      let ident = table.add_name(ident.to_name());
      guide.synthetic_object.with_children([ident]).with_data(*len)
    }
    SyntheticImpl::Struct(struct_id) => {
      let name = add_name(table, &chart.defs[chart.structs[*struct_id].def].path);
      guide.synthetic_struct.with_children([name])
    }
    SyntheticImpl::Enum(enum_id) => {
      let name = add_name(table, &chart.defs[chart.enums[*enum_id].def].path);
      guide.synthetic_enum.with_children([name])
    }
    SyntheticImpl::IfConst(const_id) => {
      let name = add_name(table, &chart.defs[chart.concrete_consts[*const_id].def].path);
      guide.synthetic_if_const.with_children([name])
    }
    SyntheticImpl::Opaque(opaque_id) => {
      let name = add_name(table, &chart.defs[chart.opaque_types[*opaque_id].def].path);
      guide.synthetic_opaque.with_children([name])
    }
  }
}

fn synthetic_item_name(
  chart: &Chart,
  guide: &Guide,
  table: &mut Table,
  item: &SyntheticItem,
) -> Name {
  match item {
    SyntheticItem::Identity => guide.synthetic_identity.to_name(),
    SyntheticItem::DebugState => guide.synthetic_debug_state.to_name(),
    SyntheticItem::CompositeDeconstruct(len) => {
      guide.synthetic_composite_deconstruct.with_data(*len)
    }
    SyntheticItem::CompositeReconstruct(len) => {
      guide.synthetic_composite_reconstruct.with_data(*len)
    }
    SyntheticItem::EnumVariantNames(enum_id) => {
      let name = add_name(table, &chart.defs[chart.enums[*enum_id].def].path);
      guide.synthetic_enum_variant_names.with_children([name])
    }
    SyntheticItem::EnumDeconstruct(enum_id) => {
      let name = add_name(table, &chart.defs[chart.enums[*enum_id].def].path);
      guide.synthetic_enum_deconstruct.with_children([name])
    }
    SyntheticItem::EnumReconstruct(enum_id) => {
      let name = add_name(table, &chart.defs[chart.enums[*enum_id].def].path);
      guide.synthetic_enum_reconstruct.with_children([name])
    }
    SyntheticItem::ConstAlias(concrete_const_id) => {
      let name = add_name(table, &chart.defs[chart.concrete_consts[*concrete_const_id].def].path);
      guide.synthetic_const_alias.with_children([name])
    }
    SyntheticItem::CallFn(fn_id, _, params) => {
      let name = add_name(table, &chart.defs[chart.fn_def(*fn_id)].path);
      guide.synthetic_call_fn.with_children([name]).with_data(*params)
    }
    SyntheticItem::Frame(path, span) => guide.synthetic_frame.with_data(frame_data(path, *span)),
    SyntheticItem::N32(n) => guide.synthetic_n32.with_data(*n),
    SyntheticItem::String(str) => guide.synthetic_string.with_data(Nat::encode_string(str)),
  }
}

fn add_path(table: &mut Table, path: &str) -> PathId {
  assert!(path.starts_with("#"));
  table.add_path(format!(":{}", &path[1..]))
}

fn add_name(table: &mut Table, path: &str) -> NameId {
  let path = add_path(table, path);
  table.add_name(path.to_name())
}
