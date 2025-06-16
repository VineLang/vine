use std::collections::HashMap;

use vine_util::{idx::IdxVec, new_idx};

use crate::structures::{
  chart::{FnId, ImplId},
  diag::ErrorGuaranteed,
  resolutions::{ConstRelId, FnRelId, FragmentId},
  tir::ClosureId,
  vir::StageId,
};

#[derive(Debug, Default)]
pub struct Specializations {
  pub lookup: IdxVec<FragmentId, HashMap<Vec<ImplTree>, SpecId>>,
  pub specs: IdxVec<SpecId, Option<Spec>>,
}

new_idx!(pub SpecId);
#[derive(Debug)]
pub struct Spec {
  pub fragment: FragmentId,
  pub index: usize,
  pub singular: bool,
  pub rels: SpecRels,
}

#[derive(Debug)]
pub struct SpecRels {
  pub fns: IdxVec<FnRelId, Result<(SpecId, StageId), ErrorGuaranteed>>,
  pub consts: IdxVec<ConstRelId, Result<SpecId, ErrorGuaranteed>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplTree {
  Error(ErrorGuaranteed),
  Def(ImplId, Vec<ImplTree>),
  Fn(FnId, Vec<ImplTree>),
  Closure(FragmentId, Vec<ImplTree>, ClosureId),
  ForkClosure(FragmentId, Vec<ImplTree>, ClosureId),
  DropClosure(FragmentId, Vec<ImplTree>, ClosureId),
}
