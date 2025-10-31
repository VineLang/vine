use std::collections::HashMap;

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
  pub lookup: IdxVec<FragmentId, HashMap<Vec<ImplTree>, SpecId>>,
  pub specs: IdxVec<SpecId, Option<Spec>>,
  pub synthetic: HashMap<(SyntheticItem, Vec<ImplTree>), SpecId>,
}

impl Specializations {
  pub fn spec(&self, spec_id: SpecId) -> &Spec {
    self.specs[spec_id].as_ref().unwrap()
  }
}

new_idx!(pub SpecId);
#[derive(Debug)]
pub struct Spec {
  pub path: String,
  pub index: usize,
  pub singular: bool,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplTree {
  Error(ErrorGuaranteed),
  Def(ImplId, Vec<ImplTree>),
  Fn(FnId, Vec<ImplTree>, usize),
  Closure(FragmentId, Vec<ImplTree>, ClosureId, usize),
  ForkClosure(FragmentId, Vec<ImplTree>, ClosureId),
  DropClosure(FragmentId, Vec<ImplTree>, ClosureId),
  Synthetic(SyntheticImpl),
}
