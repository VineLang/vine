use std::collections::HashMap;

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  components::loader::FileId,
  features::builtin::Builtins,
  structures::ast::{
    Block, Expr, Flex, Ident, Impl, ImplParam, Pat, Path, Span, Trait, Ty, TypeParam,
  },
};

#[derive(Debug, Default)]
pub struct Chart {
  pub defs: IdxVec<DefId, Def>,
  pub imports: IdxVec<ImportId, ImportDef>,
  pub generics: IdxVec<GenericsId, GenericsDef>,
  pub concrete_consts: IdxVec<ConcreteConstId, ConcreteConstDef>,
  pub concrete_fns: IdxVec<ConcreteFnId, ConcreteFnDef>,
  pub opaque_types: IdxVec<OpaqueTypeId, OpaqueTypeDef>,
  pub type_aliases: IdxVec<TypeAliasId, TypeAliasDef>,
  pub structs: IdxVec<StructId, StructDef>,
  pub enums: IdxVec<EnumId, EnumDef>,
  pub traits: IdxVec<TraitId, TraitDef>,
  pub impls: IdxVec<ImplId, ImplDef>,
  pub builtins: Builtins,
  pub main_mod: Option<DefId>,
  pub top_level: HashMap<Ident, DefId>,
  pub tests: Vec<ConcreteFnId>,
}

new_idx!(pub DefId; n => ["d{n}"]);

#[derive(Debug)]
pub struct Def {
  pub name: Ident,
  pub path: String,
  pub file: Option<FileId>,
  pub spans: Vec<Span>,

  pub members_lookup: HashMap<Ident, Binding<MemberKind>>,
  pub named_members: Vec<Binding<MemberKind>>,
  pub implicit_members: Vec<Binding<MemberKind>>,

  pub parent: Option<DefId>,
  pub ancestors: Vec<DefId>,

  pub value_kind: Option<Binding<DefValueKind>>,
  pub type_kind: Option<Binding<DefTypeKind>>,
  pub pattern_kind: Option<Binding<DefPatternKind>>,
  pub trait_kind: Option<Binding<DefTraitKind>>,
  pub impl_kind: Option<Binding<DefImplKind>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum VisId {
  Pub,
  Def(DefId),
}

#[derive(Debug, Clone, Copy)]
pub struct Binding<T> {
  pub span: Span,
  pub vis: VisId,
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

new_idx!(pub ImportId);
#[derive(Debug, Clone)]
pub struct ImportDef {
  pub span: Span,
  pub def: DefId,
  pub parent: ImportParent,
  pub ident: Ident,
}

#[derive(Debug, Clone, Copy)]
pub enum ImportParent {
  Absolute,
  Local,
  Import(ImportId),
}

new_idx!(pub GenericsId);
#[derive(Debug, Clone)]
pub struct GenericsDef {
  pub span: Span,
  pub def: DefId,
  pub parent: Option<GenericsId>,
  pub type_params: Vec<TypeParam>,
  pub impl_params: Vec<ImplParam>,
  pub impl_allowed: bool,
  pub global_flex: Flex,
  pub trait_: Option<TraitId>,
}

new_idx!(pub ConcreteConstId);
#[derive(Debug, Clone)]
pub struct ConcreteConstDef {
  pub span: Span,
  pub def: DefId,
  pub name: Ident,
  pub generics: GenericsId,
  pub ty: Ty,
  pub value: Expr,
}

new_idx!(pub ConcreteFnId);
#[derive(Debug, Clone)]
pub struct ConcreteFnDef {
  pub span: Span,
  pub def: DefId,
  pub name: Ident,
  pub generics: GenericsId,
  pub method: bool,
  pub params: Vec<Pat>,
  pub ret_ty: Option<Ty>,
  pub body: Block,
  pub frameless: bool,
}

new_idx!(pub OpaqueTypeId);
#[derive(Debug, Clone)]
pub struct OpaqueTypeDef {
  pub span: Span,
  pub def: DefId,
  pub name: Ident,
  pub generics: GenericsId,
}

new_idx!(pub TypeAliasId);
#[derive(Debug, Clone)]
pub struct TypeAliasDef {
  pub span: Span,
  pub def: DefId,
  pub name: Ident,
  pub generics: GenericsId,
  pub ty: Ty,
}

new_idx!(pub StructId);
#[derive(Debug, Clone)]
pub struct StructDef {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub name: Ident,
  pub data_vis: VisId,
  pub data: Vec<Ty>,
}

new_idx!(pub EnumId);
#[derive(Debug, Clone)]
pub struct EnumDef {
  pub span: Span,
  pub def: DefId,
  pub generics: GenericsId,
  pub name: Ident,
  pub variants: IdxVec<VariantId, EnumVariant>,
}

new_idx!(pub VariantId);
#[derive(Debug, Clone)]
pub struct EnumVariant {
  pub span: Span,
  pub def: DefId,
  pub name: Ident,
  pub data: Vec<Ty>,
}

new_idx!(pub TraitId);
#[derive(Debug, Clone)]
pub struct TraitDef {
  pub span: Span,
  pub def: DefId,
  pub name: Ident,
  pub generics: GenericsId,
  pub consts: IdxVec<TraitConstId, TraitConst>,
  pub fns: IdxVec<TraitFnId, TraitFn>,
}

new_idx!(pub TraitConstId);
#[derive(Debug, Clone)]
pub struct TraitConst {
  pub span: Span,
  pub name: Ident,
  pub generics: GenericsId,
  pub ty: Ty,
}

new_idx!(pub TraitFnId);
#[derive(Debug, Clone)]
pub struct TraitFn {
  pub span: Span,
  pub method: bool,
  pub name: Ident,
  pub generics: GenericsId,
  pub params: Vec<Pat>,
  pub ret_ty: Option<Ty>,
}

new_idx!(pub ImplId);
#[derive(Debug, Clone)]
pub struct ImplDef {
  pub span: Span,
  pub name: Ident,
  pub def: DefId,
  pub generics: GenericsId,
  pub kind: ImplDefKind,
  pub manual: bool,
  pub basic: bool,
  pub become_: Option<Path>,
}

#[derive(Debug, Clone)]
pub enum ImplDefKind {
  Direct(Trait, Vec<ImplSubitem>),
  Indirect(Trait, Option<Impl>),
  IndirectFork(DefTypeKind),
  IndirectDrop(DefTypeKind),
}

#[derive(Debug, Clone)]
pub struct ImplSubitem {
  pub span: Span,
  pub name: Ident,
  pub kind: ImplSubitemKind,
}

#[derive(Debug, Clone)]
pub enum ImplSubitemKind {
  Const(ConcreteConstId),
  Fn(ConcreteFnId),
}

impl Chart {
  pub fn visible(&self, vis: VisId, from: DefId) -> bool {
    match vis {
      VisId::Pub => true,
      VisId::Def(vis) => {
        vis == from || from != DefId::NONE && vis < from && self.defs[from].ancestors.contains(&vis)
      }
    }
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

  pub fn fn_span(&self, fn_id: FnId) -> Span {
    match fn_id {
      FnId::Concrete(fn_id) => self.concrete_fns[fn_id].span,
      FnId::Abstract(trait_id, fn_id) => self.traits[trait_id].fns[fn_id].span,
    }
  }
}

impl Def {
  pub fn fn_id(&self) -> Option<Binding<FnId>> {
    match self.value_kind {
      Some(Binding { span, vis, kind: DefValueKind::Fn(fn_id) }) => {
        Some(Binding { span, vis, kind: fn_id })
      }
      _ => None,
    }
  }
}

impl DefId {
  pub const NONE: Self = Self(usize::MAX);
}

impl GenericsId {
  pub const NONE: Self = Self(0);
}
