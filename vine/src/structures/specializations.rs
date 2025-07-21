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
pub struct Specializations<'core> {
  pub lookup: IdxVec<FragmentId, HashMap<Vec<ImplTree<'core>>, SpecId>>,
  pub specs: IdxVec<SpecId, Option<Spec<'core>>>,
  pub synthetic: HashMap<(SyntheticItem<'core>, Vec<ImplTree<'core>>), SpecId>,
}

impl<'core> Specializations<'core> {
  pub fn spec(&self, spec_id: SpecId) -> &Spec<'core> {
    self.specs[spec_id].as_ref().unwrap()
  }
}

new_idx!(pub SpecId);
#[derive(Debug)]
pub struct Spec<'core> {
  pub path: &'core str,
  pub index: usize,
  pub singular: bool,
  pub rels: SpecRels,
  pub kind: SpecKind<'core>,
}

#[derive(Debug)]
pub enum SpecKind<'core> {
  Fragment(FragmentId),
  Synthetic(SyntheticItem<'core>),
}

#[derive(Debug, Default)]
pub struct SpecRels {
  pub fns: IdxVec<FnRelId, Result<(SpecId, StageId), ErrorGuaranteed>>,
  pub consts: IdxVec<ConstRelId, Result<SpecId, ErrorGuaranteed>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplTree<'core> {
  Error(ErrorGuaranteed),
  Def(ImplId, Vec<ImplTree<'core>>),
  Fn(FnId, Vec<ImplTree<'core>>),
  Closure(FragmentId, Vec<ImplTree<'core>>, ClosureId),
  ForkClosure(FragmentId, Vec<ImplTree<'core>>, ClosureId),
  DropClosure(FragmentId, Vec<ImplTree<'core>>, ClosureId),
  Synthetic(SyntheticImpl<'core>),
}
