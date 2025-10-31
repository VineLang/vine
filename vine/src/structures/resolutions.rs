use vine_util::{idx::IdxVec, new_idx};

use crate::structures::{
  chart::{
    Chart, ConcreteConstId, ConcreteFnId, ConstId, DefId, FnId, GenericsId, ImplId, TraitConstId,
    TraitFnId, TraitId,
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

#[derive(Debug)]
pub struct ResolvedImpl {
  pub kind: ResolvedImplKind,
  pub trait_id: TraitId,
  pub become_: Become,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Become {
  Unresolved,
  Resolving,
  Resolved(Option<(ImplId, Vec<usize>)>),
}

#[derive(Debug)]
pub enum ResolvedImplKind {
  Direct {
    fns: IdxVec<TraitFnId, Result<ConcreteFnId, ErrorGuaranteed>>,
    consts: IdxVec<TraitConstId, Result<ConcreteConstId, ErrorGuaranteed>>,
  },
  Indirect(TirImpl),
}

new_idx!(pub FragmentId);
#[derive(Debug)]
pub struct Fragment {
  pub path: String,
  pub def: DefId,
  pub generics: GenericsId,
  pub impl_params: usize,
  pub tir: Tir,
  pub frameless: bool,
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
  Impl(TirImpl, usize),
}

impl Rels {
  pub fn fork_rel(&mut self, chart: &Chart, impl_: TirImpl) -> FnRelId {
    let fork = chart.builtins.fork.unwrap();
    self.fns.push(FnRel::Item(FnId::Abstract(fork, TraitFnId(0)), vec![impl_]))
  }

  pub fn drop_rel(&mut self, chart: &Chart, impl_: TirImpl) -> FnRelId {
    let drop = chart.builtins.drop.unwrap();
    self.fns.push(FnRel::Item(FnId::Abstract(drop, TraitFnId(0)), vec![impl_]))
  }
}
