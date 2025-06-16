use std::collections::HashMap;

use vine_util::{idx::IdxVec, new_idx};

use crate::structures::{
  chart::ImplId,
  diag::ErrorGuaranteed,
  resolutions::{ConstRelId, FnRelId, FragmentId},
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
  pub fns: IdxVec<FnRelId, Result<SpecId, ErrorGuaranteed>>,
  pub consts: IdxVec<ConstRelId, Result<SpecId, ErrorGuaranteed>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplTree(pub ImplId, pub Vec<ImplTree>);
