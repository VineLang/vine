use std::collections::HashMap;

use vine_util::{
  idx::{IdxVec, IntMap},
  new_idx,
};

use crate::structures::{
  ast::Ident,
  chart::{FnId, ImplId},
  diag::ErrorGuaranteed,
  resolutions::{ConstRelId, FnRelId, FragmentId},
  tir::ClosureId,
  vir::StageId,
};

#[derive(Debug, Default)]
pub struct Specializations<'core> {
  pub lookup: IdxVec<FragmentId, HashMap<Vec<ImplTree<'core>>, SpecId>>,
  pub specs: IdxVec<SpecId, Option<Spec<'core>>>,

  pub composite_deconstruct: IntMap<usize, SpecId>,
  pub composite_reconstruct: IntMap<usize, SpecId>,
  pub object_key: HashMap<Ident<'core>, SpecId>,
}

impl<'core> Specializations<'core> {
  pub fn spec(&self, spec_id: SpecId) -> &Spec<'core> {
    self.specs[spec_id].as_ref().unwrap()
  }
}

new_idx!(pub SpecId);
#[derive(Debug)]
pub struct Spec<'core> {
  pub fragment: Option<FragmentId>,
  pub path: &'core str,
  pub index: usize,
  pub singular: bool,
  pub rels: SpecRels,
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
  Tuple(usize),
  Object(Ident<'core>, usize),
}

impl<'core> Spec<'core> {
  pub fn synthetic(path: &'core str) -> Self {
    Spec { fragment: None, path, index: 0, singular: true, rels: Default::default() }
  }
}
