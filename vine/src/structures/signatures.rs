use vine_util::idx::IdxVec;

use crate::structures::{
  chart::{
    checkpoint::ChartCheckpoint, ConcreteConstId, ConcreteFnId, ConstId, DefId, EnumId, FnId,
    GenericsId, ImplId, ImportId, StructId, TraitConstId, TraitFnId, TraitId, TypeAliasId,
    VariantId,
  },
  diag::ErrorGuaranteed,
  types::{ImplType, TransferTypes, Type, TypeCtx, TypeTransfer},
};

#[derive(Debug, Default)]
pub struct Signatures<'core> {
  pub imports: IdxVec<ImportId, ImportState>,
  pub generics: IdxVec<GenericsId, TypeCtx<'core, GenericsSig>>,
  pub concrete_consts: IdxVec<ConcreteConstId, TypeCtx<'core, ConstSig>>,
  pub concrete_fns: IdxVec<ConcreteFnId, TypeCtx<'core, FnSig>>,
  pub type_aliases: IdxVec<TypeAliasId, Option<TypeCtx<'core, TypeAliasSig>>>,
  pub structs: IdxVec<StructId, TypeCtx<'core, StructSig>>,
  pub enums: IdxVec<EnumId, TypeCtx<'core, EnumSig>>,
  pub impls: IdxVec<ImplId, TypeCtx<'core, ImplSig>>,
  pub traits: IdxVec<TraitId, TraitSig<'core>>,
}

#[derive(Debug, Clone, Copy)]
pub enum ImportState {
  Unresolved,
  Resolved(Result<DefId, ErrorGuaranteed>),
  Resolving,
}

#[derive(Debug)]
pub struct GenericsSig {
  pub impl_params: Vec<ImplType>,
}

#[derive(Debug)]
pub struct ConstSig {
  pub ty: Type,
}

#[derive(Debug)]
pub struct FnSig {
  pub params: Vec<Type>,
  pub ret_ty: Type,
}

#[derive(Debug)]
pub struct TypeAliasSig {
  pub ty: Type,
}

#[derive(Debug)]
pub struct StructSig {
  pub data: Type,
}

#[derive(Debug)]
pub struct EnumSig {
  pub variant_data: IdxVec<VariantId, Option<Type>>,
}

#[derive(Debug)]
pub struct TraitSig<'core> {
  pub consts: IdxVec<TraitConstId, TypeCtx<'core, ConstSig>>,
  pub fns: IdxVec<TraitFnId, TypeCtx<'core, FnSig>>,
}

#[derive(Debug)]
pub struct ImplSig {
  pub ty: ImplType,
}

impl<'core> Signatures<'core> {
  pub fn const_sig(&self, const_id: ConstId) -> &TypeCtx<'core, ConstSig> {
    match const_id {
      ConstId::Concrete(const_id) => &self.concrete_consts[const_id],
      ConstId::Abstract(trait_id, const_id) => &self.traits[trait_id].consts[const_id],
    }
  }

  pub fn fn_sig(&self, fn_id: FnId) -> &TypeCtx<'core, FnSig> {
    match fn_id {
      FnId::Concrete(fn_id) => &self.concrete_fns[fn_id],
      FnId::Abstract(trait_id, fn_id) => &self.traits[trait_id].fns[fn_id],
    }
  }

  pub fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    let Signatures {
      imports,
      generics,
      concrete_consts,
      concrete_fns,
      type_aliases,
      structs,
      enums,
      traits,
      impls,
    } = self;
    imports.truncate(checkpoint.imports.0);
    generics.truncate(checkpoint.generics.0);
    concrete_consts.truncate(checkpoint.concrete_consts.0);
    concrete_fns.truncate(checkpoint.concrete_fns.0);
    type_aliases.truncate(checkpoint.type_aliases.0);
    structs.truncate(checkpoint.structs.0);
    enums.truncate(checkpoint.enums.0);
    traits.truncate(checkpoint.traits.0);
    impls.truncate(checkpoint.impls.0);
  }
}

impl<'core> TransferTypes<'core> for GenericsSig {
  fn transfer(&self, t: &mut TypeTransfer<'core, '_>) -> Self {
    Self { impl_params: t.transfer(&self.impl_params) }
  }
}

impl<'core> TransferTypes<'core> for ConstSig {
  fn transfer(&self, t: &mut TypeTransfer<'core, '_>) -> Self {
    Self { ty: t.transfer(&self.ty) }
  }
}

impl<'core> TransferTypes<'core> for FnSig {
  fn transfer(&self, t: &mut TypeTransfer<'core, '_>) -> Self {
    Self { params: t.transfer(&self.params), ret_ty: t.transfer(&self.ret_ty) }
  }
}

impl<'core> TransferTypes<'core> for TypeAliasSig {
  fn transfer(&self, t: &mut TypeTransfer<'core, '_>) -> Self {
    Self { ty: t.transfer(&self.ty) }
  }
}

impl<'core> TransferTypes<'core> for StructSig {
  fn transfer(&self, t: &mut TypeTransfer<'core, '_>) -> Self {
    Self { data: t.transfer(&self.data) }
  }
}

impl<'core> TransferTypes<'core> for ImplSig {
  fn transfer(&self, t: &mut TypeTransfer<'core, '_>) -> Self {
    Self { ty: t.transfer(&self.ty) }
  }
}
