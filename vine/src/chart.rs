use std::collections::HashMap;

use ivy::ast::Net;
use vine_util::{
  idx::{Counter, Idx, IdxVec},
  new_idx,
};

use crate::{
  ast::{Block, Expr, Ident, Local, Pat, Span, Trait, Ty},
  checker::Type,
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
  pub adts: IdxVec<AdtId, AdtDef<'core>>,
  pub imports: IdxVec<ImportId, Import<'core>>,
  pub builtins: Builtins,
}

#[derive(Debug, Default)]
pub struct Builtins {
  pub prelude: Option<DefId>,

  pub bool: Option<DefId>,
  pub n32: Option<DefId>,
  pub f32: Option<DefId>,
  pub char: Option<DefId>,
  pub io: Option<DefId>,

  pub list: Option<AdtId>,
  pub string: Option<AdtId>,

  pub concat: Option<ValueDefId>,

  pub to_string_trait: Option<TraitDefId>,
  pub to_string_fn: Option<ValueDefId>,
}

new_idx!(pub DefId);
new_idx!(pub ValueDefId);
new_idx!(pub TypeDefId);
new_idx!(pub PatternDefId);
new_idx!(pub TraitDefId);
new_idx!(pub ImplDefId);
new_idx!(pub GenericsId);
new_idx!(pub AdtId);
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
  Ivy {
    ty: Ty<'core>,
    net: Net,
  },
  Adt(AdtId, VariantId),
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
  Alias(Ty<'core>),
  Adt(AdtId),
  Builtin(Type<'core>),
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
  Adt(AdtId, VariantId),
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
  pub type_params: Vec<Ident<'core>>,
  pub impl_params: Vec<(Option<Ident<'core>>, Trait<'core>)>,
}

#[derive(Debug)]
pub struct AdtDef<'core> {
  pub def: DefId,
  pub generics: GenericsId,
  pub name: Ident<'core>,
  pub variants: IdxVec<VariantId, AdtVariant<'core>>,
  pub is_struct: bool,
}

#[derive(Debug)]
pub struct AdtVariant<'core> {
  pub def: DefId,
  pub name: Ident<'core>,
  pub fields: Vec<Ty<'core>>,
  pub object: bool,
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
  pub adts: AdtId,
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
      adts: self.adts.next_index(),
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
    self.adts.truncate(checkpoint.adts.0);
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
    revert_option(&mut self.n32, checkpoint.defs);
    revert_option(&mut self.f32, checkpoint.defs);
    revert_option(&mut self.char, checkpoint.defs);
    revert_option(&mut self.bool, checkpoint.defs);
    revert_option(&mut self.list, checkpoint.adts);
    revert_option(&mut self.string, checkpoint.adts);
    revert_option(&mut self.concat, checkpoint.values);
  }
}

fn revert_option<T: Idx>(option: &mut Option<T>, checkpoint: T) {
  if option.is_some_and(|id| id >= checkpoint) {
    *option = None;
  }
}

impl<'core> TypeDefKind<'core> {
  pub fn adt(&self) -> Option<AdtId> {
    if let TypeDefKind::Adt(adt) = self {
      Some(*adt)
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
