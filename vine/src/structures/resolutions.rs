use vine_util::{idx::IdxVec, new_idx};

use crate::structures::{
  chart::{
    ConcreteConstId, ConcreteFnId, ConstId, DefId, FnId, GenericsId, ImplId, TraitConstId,
    TraitFnId,
  },
  diag::ErrorGuaranteed,
  tir::{Tir, TirImpl},
};

#[derive(Debug, Default)]
pub struct Resolutions {
  pub consts: IdxVec<ConcreteConstId, FragmentId>,
  pub fns: IdxVec<ConcreteFnId, FragmentId>,
  pub impls: IdxVec<ImplId, Result<ResolvedImpl, ErrorGuaranteed>>,
  pub main: Option<FragmentId>,
}

#[derive(Debug, Default)]
pub struct ResolvedImpl {
  pub consts: IdxVec<TraitConstId, Result<ConcreteConstId, ErrorGuaranteed>>,
  pub fns: IdxVec<TraitFnId, Result<ConcreteFnId, ErrorGuaranteed>>,
}

new_idx!(pub FragmentId);
#[derive(Debug)]
pub struct Fragment<'core> {
  pub path: &'core str,
  pub def: DefId,
  pub generics: GenericsId,
  pub impl_params: usize,
  pub tir: Tir<'core>,
}

new_idx!(pub ConstRelId);
new_idx!(pub FnRelId);
#[derive(Debug, Clone, Default)]
pub struct Rels {
  pub consts: IdxVec<ConstRelId, (ConstId, Vec<TirImpl>)>,
  pub fns: IdxVec<FnRelId, FnRel>,
}

#[derive(Debug, Clone)]
pub enum FnRel {
  Item(FnId, Vec<TirImpl>),
  Impl(TirImpl),
}
