use std::collections::HashMap;

use vine_util::{
  idx::{Counter, Idx, IdxVec, IntMap},
  new_idx,
};

use crate::{
  ast::{
    BinaryOp, Block, ComparisonOp, Expr, Ident, ImplParam, Local, Pat, Span, Trait, Ty, TypeParam,
  },
  diag::ErrorGuaranteed,
};

#[derive(Debug, Default)]
pub struct Chart<'core> {
  pub defs: IdxVec<DefId, Def<'core>>,
  pub values: IdxVec<ValueDefId, ValueDef<'core>>,
  pub types: IdxVec<TypeDefId, TypeDef<'core>>,
  pub patterns: IdxVec<PatternDefId, PatternDef>,
  pub traits: IdxVec<TraitDefId, TraitDef<'core>>,
  pub impls: IdxVec<ImplDefId, ImplDef<'core>>,
  pub generics: IdxVec<GenericsId, GenericsDef<'core>>,
  pub structs: IdxVec<StructId, StructDef<'core>>,
  pub enums: IdxVec<EnumId, EnumDef<'core>>,
  pub imports: IdxVec<ImportId, Import<'core>>,
  pub builtins: Builtins,
}

#[derive(Debug, Default)]
pub struct Builtins {
  pub prelude: Option<DefId>,

  pub bool: Option<TypeDefId>,
  pub n32: Option<TypeDefId>,
  pub i32: Option<TypeDefId>,
  pub f32: Option<TypeDefId>,
  pub char: Option<TypeDefId>,
  pub io: Option<TypeDefId>,

  pub list: Option<StructId>,
  pub string: Option<StructId>,
  pub result: Option<EnumId>,

  pub neg: Option<ValueDefId>,
  pub not: Option<ValueDefId>,
  pub bool_not: Option<ImplDefId>,
  pub cast: Option<ValueDefId>,
  pub binary_ops: IntMap<BinaryOp, Option<ValueDefId>>,
  pub comparison_ops: IntMap<ComparisonOp, Option<ValueDefId>>,

  pub fork: Option<TraitDefId>,
  pub drop: Option<TraitDefId>,

  pub range: Option<StructId>,
  pub bound_exclusive: Option<StructId>,
  pub bound_inclusive: Option<StructId>,
  pub unbounded: Option<StructId>,
}

new_idx!(pub DefId);
new_idx!(pub ValueDefId);
new_idx!(pub TypeDefId);
new_idx!(pub PatternDefId);
new_idx!(pub TraitDefId);
new_idx!(pub ImplDefId);
new_idx!(pub GenericsId);
new_idx!(pub StructId);
new_idx!(pub EnumId);
new_idx!(pub VariantId);
new_idx!(pub SubitemId);
new_idx!(pub ImportId);

impl DefId {
  pub const ROOT: Self = Self(0);
}

impl GenericsId {
  pub const NONE: Self = Self(0);
}

#[derive(Debug)]
pub struct Def<'core> {
  pub name: Ident<'core>,
  pub path: &'core str,

  pub members: HashMap<Ident<'core>, Member>,
  pub parent: Option<DefId>,
  pub ancestors: Vec<DefId>,

  pub value_def: Option<ValueDefId>,
  pub type_def: Option<TypeDefId>,
  pub pattern_def: Option<PatternDefId>,
  pub trait_def: Option<TraitDefId>,
  pub impl_def: Option<ImplDefId>,
}

#[derive(Debug)]
pub struct Member {
  pub vis: DefId,
  pub kind: MemberKind,
}

#[derive(Debug)]
pub enum MemberKind {
  Child(DefId),
  Import(ImportId),
}

#[derive(Debug, Clone, Copy)]
pub struct Import<'core> {
  pub span: Span,
  pub def: DefId,
  pub parent: ImportParent,
  pub ident: Ident<'core>,
  pub state: ImportState,
}

#[derive(Debug, Clone, Copy)]
pub enum ImportParent {
  Root,
  Scope,
  Import(ImportId),
}

#[derive(Debug, Clone, Copy)]
pub enum ImportState {
  Unresolved,
  Resolved(Result<DefId, ErrorGuaranteed>),
  Resolving,
}

#[derive(Debug)]
pub struct ValueDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub vis: DefId,
  pub generics: GenericsId,
  pub locals: Counter<Local>,
  pub kind: ValueDefKind<'core>,
  pub method: bool,
}

#[derive(Debug, Default)]
pub enum ValueDefKind<'core> {
  #[default]
  Taken,
  Const {
    ty: Ty<'core>,
    value: Expr<'core>,
  },
  Fn {
    params: Vec<Pat<'core>>,
    ret: Option<Ty<'core>>,
    body: Block<'core>,
  },
  Struct(StructId),
  Enum(EnumId, VariantId),
  TraitSubitem(TraitDefId, SubitemId),
}

#[derive(Debug)]
pub struct TypeDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub vis: DefId,
  pub generics: GenericsId,
  pub kind: TypeDefKind<'core>,
}

#[derive(Debug, Default)]
pub enum TypeDefKind<'core> {
  #[default]
  Taken,
  Opaque,
  Alias(Ty<'core>),
  Struct(StructId),
  Enum(EnumId),
}

#[derive(Debug)]
pub struct PatternDef {
  pub span: Span,
  pub def: DefId,
  pub vis: DefId,
  pub generics: GenericsId,
  pub kind: PatternDefKind,
}

#[derive(Debug)]
pub enum PatternDefKind {
  Struct(StructId),
  Enum(EnumId, VariantId),
}

#[derive(Debug)]
pub struct TraitDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub vis: DefId,
  pub generics: GenericsId,
  pub kind: TraitDefKind<'core>,
}

#[derive(Debug, Default)]
pub enum TraitDefKind<'core> {
  #[default]
  Taken,
  Trait {
    subitems: IdxVec<SubitemId, TraitSubitem<'core>>,
  },
}

#[derive(Debug)]
pub struct TraitSubitem<'core> {
  pub name: Ident<'core>,
  pub kind: TraitSubitemKind<'core>,
}

#[derive(Debug)]
pub enum TraitSubitemKind<'core> {
  Fn(Vec<Pat<'core>>, Option<Ty<'core>>),
  Const(Ty<'core>),
}

#[derive(Debug)]
pub struct ImplDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub vis: DefId,
  pub generics: GenericsId,
  pub kind: ImplDefKind<'core>,
}

#[derive(Debug, Default)]
pub enum ImplDefKind<'core> {
  #[default]
  Taken,
  Impl {
    trait_: Trait<'core>,
    subitems: IdxVec<SubitemId, ImplSubitem<'core>>,
  },
}

#[derive(Debug)]
pub struct ImplSubitem<'core> {
  pub span: Span,
  pub name: Ident<'core>,
  pub value: ValueDefId,
}

#[derive(Debug, Default, Clone)]
pub struct GenericsDef<'core> {
  pub span: Span,
  pub def: DefId,
  pub type_params: Vec<TypeParam<'core>>,
  pub impl_params: Vec<ImplParam<'core>>,
}

#[derive(Debug)]
pub struct StructDef<'core> {
  pub def: DefId,
  pub generics: GenericsId,
  pub name: Ident<'core>,
  pub data_vis: DefId,
  pub data: Ty<'core>,
}

#[derive(Debug)]
pub struct EnumDef<'core> {
  pub def: DefId,
  pub generics: GenericsId,
  pub name: Ident<'core>,
  pub variants: IdxVec<VariantId, EnumVariant<'core>>,
}

#[derive(Debug)]
pub struct EnumVariant<'core> {
  pub def: DefId,
  pub name: Ident<'core>,
  pub data: Option<Ty<'core>>,
}

#[derive(Default, Debug)]
pub struct ChartCheckpoint {
  pub defs: DefId,
  pub values: ValueDefId,
  pub types: TypeDefId,
  pub patterns: PatternDefId,
  pub traits: TraitDefId,
  pub impls: ImplDefId,
  pub generics: GenericsId,
  pub structs: StructId,
  pub enums: EnumId,
  pub imports: ImportId,
}

impl<'core> Chart<'core> {
  pub fn checkpoint(&self) -> ChartCheckpoint {
    ChartCheckpoint {
      defs: self.defs.next_index(),
      values: self.values.next_index(),
      types: self.types.next_index(),
      patterns: self.patterns.next_index(),
      traits: self.traits.next_index(),
      impls: self.impls.next_index(),
      generics: self.generics.next_index(),
      structs: self.structs.next_index(),
      enums: self.enums.next_index(),
      imports: self.imports.next_index(),
    }
  }

  pub fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    self.defs.truncate(checkpoint.defs.0);
    self.values.truncate(checkpoint.values.0);
    self.types.truncate(checkpoint.types.0);
    self.patterns.truncate(checkpoint.patterns.0);
    self.traits.truncate(checkpoint.traits.0);
    self.impls.truncate(checkpoint.impls.0);
    self.generics.truncate(checkpoint.generics.0);
    self.structs.truncate(checkpoint.structs.0);
    self.enums.truncate(checkpoint.enums.0);
    self.imports.truncate(checkpoint.imports.0);

    for def in self.defs.values_mut() {
      def.revert(checkpoint);
    }

    self.builtins.revert(checkpoint);
  }
}

impl<'core> Def<'core> {
  fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    self.members.retain(|_, member| match member.kind {
      MemberKind::Child(id) => id < checkpoint.defs,
      MemberKind::Import(id) => id < checkpoint.imports,
    });
    revert_option(&mut self.value_def, checkpoint.values);
    revert_option(&mut self.type_def, checkpoint.types);
    revert_option(&mut self.pattern_def, checkpoint.patterns);
    revert_option(&mut self.trait_def, checkpoint.traits);
    revert_option(&mut self.impl_def, checkpoint.impls);
  }
}

impl Builtins {
  fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    revert_option(&mut self.prelude, checkpoint.defs);
    revert_option(&mut self.bool, checkpoint.types);
    revert_option(&mut self.n32, checkpoint.types);
    revert_option(&mut self.f32, checkpoint.types);
    revert_option(&mut self.i32, checkpoint.types);
    revert_option(&mut self.char, checkpoint.types);
    revert_option(&mut self.io, checkpoint.types);
    revert_option(&mut self.list, checkpoint.structs);
    revert_option(&mut self.string, checkpoint.structs);
    revert_option(&mut self.neg, checkpoint.values);
    revert_option(&mut self.not, checkpoint.values);
    revert_option(&mut self.bool_not, checkpoint.impls);
    revert_option(&mut self.cast, checkpoint.values);
    self.binary_ops.values_mut().for_each(|op| revert_option(op, checkpoint.values));
    self.comparison_ops.values_mut().for_each(|op| revert_option(op, checkpoint.values));
  }
}

fn revert_option<T: Idx>(option: &mut Option<T>, checkpoint: T) {
  if option.is_some_and(|id| id >= checkpoint) {
    *option = None;
  }
}

impl<'core> TypeDefKind<'core> {
  pub fn struct_id(&self) -> Option<StructId> {
    if let TypeDefKind::Struct(struct_id) = self {
      Some(*struct_id)
    } else {
      None
    }
  }

  pub fn enum_id(&self) -> Option<EnumId> {
    if let TypeDefKind::Enum(enum_id) = self {
      Some(*enum_id)
    } else {
      None
    }
  }
}

impl<'core> Chart<'core> {
  pub fn visible(&self, vis: DefId, from: DefId) -> bool {
    vis == from || vis < from && self.defs[from].ancestors.binary_search(&vis).is_ok()
  }
}

impl<'core> Import<'core> {
  pub fn resolved(&self) -> Option<DefId> {
    if let ImportState::Resolved(Ok(def_id)) = self.state {
      Some(def_id)
    } else {
      None
    }
  }
}
