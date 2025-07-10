use std::collections::HashMap;

use vine_util::idx::IdxVec;

use crate::structures::{
  ast::Ident,
  chart::{
    ConcreteConstId, ConcreteFnId, ConstId, DefId, EnumId, FnId, GenericsId, ImplId, ImportId,
    StructId, TraitConstId, TraitFnId, TraitId, TypeAliasId, VariantId,
  },
  diag::ErrorGuaranteed,
  types::{ImplType, TransferTypes, Type, TypeCtx, TypeTransfer},
};

#[derive(Debug, Default)]
pub struct Signatures<'core> {
  pub imports: IdxVec<ImportId, ImportState>,
  pub type_params: IdxVec<GenericsId, TypeParams<'core>>,
  pub impl_params: IdxVec<GenericsId, ImplParams<'core>>,
  pub concrete_consts: IdxVec<ConcreteConstId, TypeCtx<'core, ConstSig>>,
  pub concrete_fns: IdxVec<ConcreteFnId, TypeCtx<'core, FnSig>>,
  pub type_aliases: IdxVec<TypeAliasId, TypeAliasState<'core>>,
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

#[derive(Debug, Default)]
pub enum TypeAliasState<'core> {
  #[default]
  Unresolved,
  Resolving,
  Resolved(TypeCtx<'core, TypeAliasSig>),
}

#[derive(Debug, Clone, Default)]
pub struct TypeParams<'core> {
  pub count: usize,
  pub lookup: HashMap<Ident<'core>, usize>,
}

#[derive(Debug, Clone, Default)]
pub struct ImplParams<'core> {
  pub types: TypeCtx<'core, Vec<ImplType>>,
  pub lookup: HashMap<Ident<'core>, usize>,
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

impl<'core> TransferTypes<'core> for EnumSig {
  fn transfer(&self, t: &mut TypeTransfer<'core, '_>) -> Self {
    Self { variant_data: t.transfer(&self.variant_data) }
  }
}

impl<'core> TransferTypes<'core> for ImplSig {
  fn transfer(&self, t: &mut TypeTransfer<'core, '_>) -> Self {
    Self { ty: t.transfer(&self.ty) }
  }
}
