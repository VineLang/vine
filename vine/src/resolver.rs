use std::collections::HashMap;

use ivy::ast::Net;
use vine_util::{
  idx::{Counter, IdxVec},
  new_idx,
};

use crate::{
  ast::{Builtin, Expr, Ident, Path, Span, Ty},
  checker::Type,
  diag::DiagGroup,
};

mod build_graph;
mod resolve_defs;
mod resolve_path;

#[derive(Debug, Default)]
pub struct Resolver {
  pub defs: IdxVec<DefId, Def>,
  pub diags: DiagGroup,
  pub use_id: Counter<UseId>,
  pub builtins: HashMap<Builtin, DefId>,
}

new_idx!(pub DefId);
new_idx!(pub UseId);

impl DefId {
  pub const ROOT: Self = Self(0);
}

#[derive(Debug)]
pub struct Def {
  pub id: DefId,
  pub canonical: Path,

  pub value_def: Option<ValueDef>,
  pub type_def: Option<TypeDef>,
  pub adt_def: Option<AdtDef>,
  pub variant_def: Option<VariantDef>,

  members: HashMap<Ident, Member>,
  parent: Option<DefId>,
  ancestors: Vec<DefId>,
}

#[derive(Debug)]
struct Member {
  pub vis: DefId,
  pub kind: MemberKind,
}

#[derive(Debug)]
enum MemberKind {
  Child(DefId),
  ResolvedImport(DefId, UseId),
  UnresolvedImport(Span, Option<Path>, UseId),
}

#[derive(Debug)]
pub struct ValueDef {
  pub vis: DefId,
  pub generics: Vec<Ident>,
  pub annotation: Option<Ty>,
  pub ty: Option<Type>,
  pub locals: usize,
  pub kind: ValueDefKind,
}

#[derive(Debug)]
pub enum ValueDefKind {
  Expr(Expr),
  Ivy(Net),
  AdtConstructor,
}

#[derive(Debug)]
pub struct TypeDef {
  pub vis: DefId,
  pub generics: Vec<Ident>,
  pub alias: Option<Ty>,
  pub ty: Option<Type>,
}

#[derive(Debug)]
pub struct AdtDef {
  pub generics: Vec<Ident>,
  pub variants: Vec<DefId>,
}

#[derive(Debug)]
pub struct VariantDef {
  pub vis: DefId,
  pub generics: Vec<Ident>,
  pub adt: DefId,
  pub variant: usize,
  pub fields: Vec<Ty>,
  pub field_types: Option<Vec<Type>>,
}
