use vine_util::{idx::IdxVec, new_idx};

use crate::structures::{
  chart::{
    Chart, ConcreteConstId, ConcreteFnId, ConstId, DefId, FnId, GenericsId, ImplId, TraitConstId,
    TraitFnId,
  },
  diag::ErrorGuaranteed,
  tir::{Tir, TirImpl},
};

#[derive(Debug, Default)]
pub struct Resolutions<'core> {
  pub consts: IdxVec<ConcreteConstId, FragmentId>,
  pub fns: IdxVec<ConcreteFnId, FragmentId>,
  pub impls: IdxVec<ImplId, Result<ResolvedImpl<'core>, ErrorGuaranteed>>,
  pub main: Option<FragmentId>,
}

#[derive(Debug)]
pub struct ResolvedImpl<'core> {
  pub is_fork: bool,
  pub is_drop: bool,
  pub kind: ResolvedImplKind<'core>,
}

#[derive(Debug)]
pub enum ResolvedImplKind<'core> {
  Direct {
    fns: IdxVec<TraitFnId, Result<ConcreteFnId, ErrorGuaranteed>>,
    consts: IdxVec<TraitConstId, Result<ConcreteConstId, ErrorGuaranteed>>,
  },
  Indirect(TirImpl<'core>),
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
pub struct Rels<'core> {
  pub consts: IdxVec<ConstRelId, (ConstId, Vec<TirImpl<'core>>)>,
  pub fns: IdxVec<FnRelId, FnRel<'core>>,
}

#[derive(Debug, Clone)]
pub enum FnRel<'core> {
  Item(FnId, Vec<TirImpl<'core>>),
  Impl(TirImpl<'core>),
}

impl<'core> Rels<'core> {
  pub fn fork_rel(&mut self, chart: &Chart<'core>, impl_: TirImpl<'core>) -> FnRelId {
    let fork = chart.builtins.fork.unwrap();
    self.fns.push(FnRel::Item(FnId::Abstract(fork, TraitFnId(0)), vec![impl_]))
  }

  pub fn drop_rel(&mut self, chart: &Chart<'core>, impl_: TirImpl<'core>) -> FnRelId {
    let drop = chart.builtins.drop.unwrap();
    self.fns.push(FnRel::Item(FnId::Abstract(drop, TraitFnId(0)), vec![impl_]))
  }
}
