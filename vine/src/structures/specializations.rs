use std::collections::HashMap;

use hedera::name::{Name, NameId};
use vine_util::{idx::IdxVec, new_idx};

use crate::{
  components::synthesizer::{SyntheticImpl, SyntheticItem},
  structures::{
    chart::{FnId, ImplId},
    diag::ErrorGuaranteed,
    resolutions::{ConstRelId, FnRelId, FragmentId},
    tir::ClosureId,
    vir::StageId,
  },
};

#[derive(Debug, Default)]
pub struct Specializations {
  pub impls: IdxVec<ImplTreeId, ImplTree>,
  pub impl_lookup: HashMap<ImplTreeKind, ImplTreeId>,

  pub lookup: IdxVec<FragmentId, HashMap<Vec<ImplTreeId>, SpecId>>,
  pub specs: IdxVec<SpecId, Option<Spec>>,

  pub synthetic: HashMap<(SyntheticItem, Vec<ImplTreeId>), SpecId>,
}

impl Specializations {
  pub fn spec(&self, spec_id: SpecId) -> &Spec {
    self.specs[spec_id].as_ref().unwrap()
  }
}

new_idx!(pub SpecId);
#[derive(Debug)]
pub struct Spec {
  pub name: Name,
  pub rels: SpecRels,
  pub kind: SpecKind,
}

#[derive(Debug)]
pub enum SpecKind {
  Fragment(FragmentId),
  Synthetic(SyntheticItem),
}

#[derive(Debug, Default)]
pub struct SpecRels {
  pub fns: IdxVec<FnRelId, Result<(SpecId, StageId), ErrorGuaranteed>>,
  pub consts: IdxVec<ConstRelId, Result<SpecId, ErrorGuaranteed>>,
}

new_idx!(pub ImplTreeId);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplTree {
  pub name: NameId,
  pub kind: ImplTreeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplTreeKind {
  Error(ErrorGuaranteed),
  Def(ImplId, Vec<ImplTreeId>),
  Fn(FnId, Vec<ImplTreeId>, usize),
  Closure(FragmentId, Vec<ImplTreeId>, ClosureId, usize),
  ForkClosure(FragmentId, Vec<ImplTreeId>, ClosureId),
  DropClosure(FragmentId, Vec<ImplTreeId>, ClosureId),
  Synthetic(SyntheticImpl),
}
