use std::collections::HashMap;

use ivy::ast::Net;
use vine_util::{
  idx::{Counter, IdxVec},
  new_idx,
};

use crate::{
  ast::{Builtin, Expr, Ident, Local, Path, Span, Ty},
  checker::Type,
  core::Core,
};

mod build_graph;
mod resolve_defs;
mod resolve_path;

#[derive(Debug)]
pub struct Resolver<'core> {
  pub core: &'core Core<'core>,
  pub defs: IdxVec<DefId, Def<'core>>,
  pub use_id: Counter<UseId>,
  pub builtins: HashMap<Builtin, DefId>,
}

impl<'core> Resolver<'core> {
  pub fn new(core: &'core Core<'core>) -> Self {
    Resolver {
      core,
      defs: Default::default(),
      use_id: Default::default(),
      builtins: Default::default(),
    }
  }
}

new_idx!(pub DefId);
new_idx!(pub UseId);

impl DefId {
  pub const ROOT: Self = Self(0);
}

#[derive(Debug)]
pub struct Def<'core> {
  pub id: DefId,
  pub canonical: Path<'core>,

  pub value_def: Option<ValueDef<'core>>,
  pub type_def: Option<TypeDef<'core>>,
  pub adt_def: Option<AdtDef<'core>>,
  pub variant_def: Option<VariantDef<'core>>,

  members: HashMap<Ident<'core>, Member<'core>>,
  parent: Option<DefId>,
  ancestors: Vec<DefId>,
}

#[derive(Debug)]
struct Member<'core> {
  pub vis: DefId,
  pub kind: MemberKind<'core>,
}

#[derive(Debug)]
enum MemberKind<'core> {
  Child(DefId),
  ResolvedImport(DefId, UseId),
  UnresolvedImport(Span, Option<Path<'core>>, UseId),
}

#[derive(Debug)]
pub struct ValueDef<'core> {
  pub vis: DefId,
  pub generics: Vec<Ident<'core>>,
  pub annotation: Option<Ty<'core>>,
  pub ty: Option<Type<'core>>,
  pub locals: Counter<Local>,
  pub kind: ValueDefKind<'core>,
}

#[derive(Debug)]
pub enum ValueDefKind<'core> {
  Expr(Expr<'core>),
  Ivy(Net),
  AdtConstructor,
}

#[derive(Debug)]
pub struct TypeDef<'core> {
  pub vis: DefId,
  pub generics: Vec<Ident<'core>>,
  pub alias: Option<Ty<'core>>,
  pub ty: Option<Type<'core>>,
}

#[derive(Debug)]
pub struct AdtDef<'core> {
  pub generics: Vec<Ident<'core>>,
  pub variants: Vec<DefId>,
}

#[derive(Debug)]
pub struct VariantDef<'core> {
  pub vis: DefId,
  pub generics: Vec<Ident<'core>>,
  pub adt: DefId,
  pub variant: usize,
  pub fields: Vec<Ty<'core>>,
  pub field_types: Option<Vec<Type<'core>>>,
}
