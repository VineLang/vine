use std::collections::HashMap;

use vine_util::idx::IdxVec;

use crate::structures::{
  ast::Ident,
  chart::{
    ConcreteConstId, ConcreteFnId, ConstId, DefId, EnumId, FnId, GenericsId, ImplId, ImportId,
    MemberKind, StructId, TraitConstId, TraitFnId, TraitId, TypeAliasId, VariantId,
  },
  diag::ErrorGuaranteed,
  types::{ImplType, TransferTypes, Type, TypeCtx, TypeTransfer},
};

#[derive(Debug, Default)]
pub struct Signatures {
  pub imports: IdxVec<ImportId, ImportState>,
  pub type_params: IdxVec<GenericsId, TypeParams>,
  pub impl_params: IdxVec<GenericsId, ImplParams>,
  pub concrete_consts: IdxVec<ConcreteConstId, TypeCtx<ConstSig>>,
  pub concrete_fns: IdxVec<ConcreteFnId, TypeCtx<FnSig>>,
  pub type_aliases: IdxVec<TypeAliasId, TypeAliasState>,
  pub structs: IdxVec<StructId, TypeCtx<StructSig>>,
  pub enums: IdxVec<EnumId, TypeCtx<EnumSig>>,
  pub impls: IdxVec<ImplId, TypeCtx<ImplSig>>,
  pub traits: IdxVec<TraitId, TraitSig>,
}

#[derive(Debug, Clone, Copy)]
pub enum ImportState {
  Unresolved,
  Resolved(Result<DefId, ErrorGuaranteed>),
  Resolving,
}

#[derive(Debug, Default)]
pub enum TypeAliasState {
  #[default]
  Unresolved,
  Resolving,
  Resolved(TypeCtx<TypeAliasSig>),
}

#[derive(Debug, Clone, Default)]
pub struct TypeParams {
  pub params: Vec<Ident>,
  pub lookup: HashMap<Ident, usize>,
}

#[derive(Debug, Clone, Default)]
pub struct ImplParams {
  pub types: TypeCtx<Vec<ImplType>>,
  pub lookup: HashMap<Ident, usize>,
}

#[derive(Debug)]
pub struct ConstSig {
  pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FnSig {
  pub names: Vec<Option<Ident>>,
  pub param_tys: Vec<Type>,
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
  pub variant_data: IdxVec<VariantId, Type>,
  pub variant_is_nil: IdxVec<VariantId, bool>,
}

#[derive(Debug)]
pub struct TraitSig {
  pub consts: IdxVec<TraitConstId, TypeCtx<ConstSig>>,
  pub fns: IdxVec<TraitFnId, TypeCtx<FnSig>>,
}

#[derive(Debug)]
pub struct ImplSig {
  pub ty: ImplType,
}

impl Signatures {
  pub fn const_sig(&self, const_id: ConstId) -> &TypeCtx<ConstSig> {
    match const_id {
      ConstId::Concrete(const_id) => &self.concrete_consts[const_id],
      ConstId::Abstract(trait_id, const_id) => &self.traits[trait_id].consts[const_id],
    }
  }

  pub fn fn_sig(&self, fn_id: FnId) -> &TypeCtx<FnSig> {
    match fn_id {
      FnId::Concrete(fn_id) => &self.concrete_fns[fn_id],
      FnId::Abstract(trait_id, fn_id) => &self.traits[trait_id].fns[fn_id],
    }
  }

  pub fn get_member(&self, member: MemberKind) -> Option<DefId> {
    match member {
      MemberKind::Child(def_id) => Some(def_id),
      MemberKind::Import(import_id) => match self.imports[import_id] {
        ImportState::Resolved(Ok(def_id)) => Some(def_id),
        _ => None,
      },
    }
  }
}

impl TransferTypes for ConstSig {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    Self { ty: t.transfer(&self.ty) }
  }
}

impl TransferTypes for FnSig {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    Self {
      names: self.names.clone(),
      param_tys: t.transfer(&self.param_tys),
      ret_ty: t.transfer(&self.ret_ty),
    }
  }
}

impl TransferTypes for TypeAliasSig {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    Self { ty: t.transfer(&self.ty) }
  }
}

impl TransferTypes for StructSig {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    Self { data: t.transfer(&self.data) }
  }
}

impl TransferTypes for EnumSig {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    Self {
      variant_data: t.transfer(&self.variant_data),
      variant_is_nil: self.variant_is_nil.clone(),
    }
  }
}

impl TransferTypes for ImplSig {
  fn transfer(&self, t: &mut TypeTransfer<'_>) -> Self {
    Self { ty: t.transfer(&self.ty) }
  }
}
