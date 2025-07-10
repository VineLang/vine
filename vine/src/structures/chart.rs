use std::collections::HashMap;

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  features::builtin::Builtins,
  structures::ast::{Block, Expr, Ident, Impl, ImplParam, Pat, Span, Trait, Ty, TypeParam},
};

#[derive(Debug, Default)]
pub struct Chart<'core> {
  pub defs: IdxVec<DefId, Def<'core>>,
  pub imports: IdxVec<ImportId, ImportDef<'core>>,
  pub generics: IdxVec<GenericsId, GenericsDef<'core>>,
  pub concrete_consts: IdxVec<ConcreteConstId, ConcreteConstDef<'core>>,
  pub concrete_fns: IdxVec<ConcreteFnId, ConcreteFnDef<'core>>,
  pub opaque_types: IdxVec<OpaqueTypeId, OpaqueTypeDef<'core>>,
  pub type_aliases: IdxVec<TypeAliasId, TypeAliasDef<'core>>,
  pub structs: IdxVec<StructId, StructDef<'core>>,
  pub enums: IdxVec<EnumId, EnumDef<'core>>,
  pub traits: IdxVec<TraitId, TraitDef<'core>>,
  pub direct_impls: IdxVec<DirectImplId, DirectImplDef<'core>>,
  pub indirect_impls: IdxVec<IndirectImplId, IndirectImplDef<'core>>,
  pub builtins: Builtins,
  pub main_mod: Option<DefId>,
}

new_idx!(pub DefId);
#[derive(Debug)]
pub struct Def<'core> {
  pub name: Ident<'core>,
  pub path: &'core str,

  pub members: HashMap<Ident<'core>, WithVis<MemberKind>>,
  pub all_members: Vec<WithVis<MemberKind>>,

  pub parent: Option<DefId>,
  pub ancestors: Vec<DefId>,

  pub value_kind: Option<WithVis<DefValueKind>>,
  pub type_kind: Option<WithVis<DefTypeKind>>,
  pub pattern_kind: Option<WithVis<DefPatternKind>>,
  pub trait_kind: Option<WithVis<DefTraitKind>>,
  pub impl_kind: Option<WithVis<DefImplKind>>,
}

#[derive(Debug, Clone, Copy)]
pub struct WithVis<T> {
  pub vis: DefId,
  pub kind: T,
}

#[derive(Debug, Clone, Copy)]
pub enum MemberKind {
  Child(DefId),
  Import(ImportId),
}

#[derive(Debug, Clone, Copy)]
pub enum DefValueKind {
  Const(ConstId),
  Fn(FnId),
  Struct(StructId),
  Enum(EnumId, VariantId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstId {
  Concrete(ConcreteConstId),
  Abstract(TraitId, TraitConstId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FnId {
  Concrete(ConcreteFnId),
  Abstract(TraitId, TraitFnId),
}

#[derive(Debug, Clone, Copy)]
pub enum DefTypeKind {
  Opaque(OpaqueTypeId),
  Alias(TypeAliasId),
  Struct(StructId),
  Enum(EnumId),
}

#[derive(Debug, Clone, Copy)]
pub enum DefPatternKind {
  Struct(StructId),
  Enum(EnumId, VariantId),
}

#[derive(Debug, Clone, Copy)]
pub enum DefTraitKind {
  Trait(TraitId),
}

#[derive(Debug, Clone, Copy)]
pub enum DefImplKind {
  Impl(ImplId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplId {
  Direct(DirectImplId),
  Indirect(IndirectImplId),
}

new_idx!(pub ImportId);
#[derive(Debug, Clone, Copy)]
pub struct ImportDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub parent: ImportParent,
  pub ident: Ident<'core>,
}

#[derive(Debug, Clone, Copy)]
pub enum ImportParent {
  Root,
  Scope,
  Import(ImportId),
}

new_idx!(pub GenericsId);
#[derive(Debug, Clone)]
pub struct GenericsDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub parent: Option<GenericsId>,
  pub type_params: Vec<TypeParam<'core>>,
  pub impl_params: Vec<ImplParam<'core>>,
  pub impl_allowed: bool,
  pub trait_: Option<TraitId>,
}

new_idx!(pub ConcreteConstId);
#[derive(Debug, Clone)]
pub struct ConcreteConstDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub ty: Ty<'core>,
  pub value: Expr<'core>,
}

new_idx!(pub ConcreteFnId);
#[derive(Debug, Clone)]
pub struct ConcreteFnDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub method: bool,
  pub params: Vec<Pat<'core>>,
  pub ret_ty: Option<Ty<'core>>,
  pub body: Block<'core>,
}

new_idx!(pub OpaqueTypeId);
#[derive(Debug, Clone)]
pub struct OpaqueTypeDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub name: Ident<'core>,
}

new_idx!(pub TypeAliasId);
#[derive(Debug, Clone)]
pub struct TypeAliasDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub ty: Ty<'core>,
}

new_idx!(pub StructId);
#[derive(Debug, Clone)]
pub struct StructDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub name: Ident<'core>,
  pub data_vis: DefId,
  pub data: Ty<'core>,
}

new_idx!(pub EnumId);
#[derive(Debug, Clone)]
pub struct EnumDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub name: Ident<'core>,
  pub variants: IdxVec<VariantId, EnumVariant<'core>>,
}

new_idx!(pub VariantId);
#[derive(Debug, Clone)]
pub struct EnumVariant<'core> {
  pub span: Span,
  pub def: DefId,
  pub name: Ident<'core>,
  pub data: Option<Ty<'core>>,
}

new_idx!(pub TraitId);
#[derive(Debug, Clone)]
pub struct TraitDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub name: Ident<'core>,
  pub generics: GenericsId,
  pub consts: IdxVec<TraitConstId, TraitConst<'core>>,
  pub fns: IdxVec<TraitFnId, TraitFn<'core>>,
}

new_idx!(pub TraitConstId);
#[derive(Debug, Clone)]
pub struct TraitConst<'core> {
  pub name: Ident<'core>,
  pub generics: GenericsId,
  pub ty: Ty<'core>,
}

new_idx!(pub TraitFnId);
#[derive(Debug, Clone)]
pub struct TraitFn<'core> {
  pub method: bool,
  pub name: Ident<'core>,
  pub generics: GenericsId,
  pub params: Vec<Pat<'core>>,
  pub ret_ty: Option<Ty<'core>>,
}

new_idx!(pub DirectImplId);
#[derive(Debug, Clone)]
pub struct DirectImplDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub trait_: Trait<'core>,
  pub subitems: Vec<ImplSubitem<'core>>,
  pub manual: bool,
  pub duplicate: bool,
  pub erase: bool,
}

new_idx!(pub IndirectImplId);
#[derive(Debug, Clone)]
pub struct IndirectImplDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub trait_: Trait<'core>,
  pub impl_: Impl<'core>,
  pub manual: bool,
}

#[derive(Debug, Clone)]
pub struct ImplSubitem<'core> {
  pub span: Span,
  pub name: Ident<'core>,
  pub kind: ImplSubitemKind,
}

#[derive(Debug, Clone)]
pub enum ImplSubitemKind {
  Const(ConcreteConstId),
  Fn(ConcreteFnId),
}

impl<'core> Chart<'core> {
  pub fn visible(&self, vis: DefId, from: DefId) -> bool {
    vis == from || vis < from && self.defs[from].ancestors.contains(&vis)
  }

  pub fn fn_is_method(&self, fn_id: FnId) -> bool {
    match fn_id {
      FnId::Concrete(fn_id) => self.concrete_fns[fn_id].method,
      FnId::Abstract(trait_id, fn_id) => self.traits[trait_id].fns[fn_id].method,
    }
  }

  pub fn fn_generics(&self, fn_id: FnId) -> GenericsId {
    match fn_id {
      FnId::Concrete(fn_id) => self.concrete_fns[fn_id].generics,
      FnId::Abstract(trait_id, fn_id) => self.traits[trait_id].fns[fn_id].generics,
    }
  }

  pub fn const_generics(&self, const_id: ConstId) -> GenericsId {
    match const_id {
      ConstId::Concrete(const_id) => self.concrete_consts[const_id].generics,
      ConstId::Abstract(trait_id, const_id) => self.traits[trait_id].consts[const_id].generics,
    }
  }

  pub fn impl_is_manual(&self, impl_id: ImplId) -> bool {
    match impl_id {
      ImplId::Direct(impl_id) => self.direct_impls[impl_id].manual,
      ImplId::Indirect(impl_id) => self.indirect_impls[impl_id].manual,
    }
  }

  pub fn impl_generics(&self, impl_id: ImplId) -> GenericsId {
    match impl_id {
      ImplId::Direct(impl_id) => self.direct_impls[impl_id].generics,
      ImplId::Indirect(impl_id) => self.indirect_impls[impl_id].generics,
    }
  }
}

impl<'core> Def<'core> {
  pub fn fn_id(&self) -> Option<WithVis<FnId>> {
    match self.value_kind {
      Some(WithVis { vis, kind: DefValueKind::Fn(fn_id) }) => Some(WithVis { vis, kind: fn_id }),
      _ => None,
    }
  }
}

impl DefId {
  pub const ROOT: Self = Self(0);
}

impl GenericsId {
  pub const NONE: Self = Self(0);
}
