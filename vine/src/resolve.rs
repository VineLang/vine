use std::collections::HashMap;

use ivy::ast::Net;

use crate::{
  ast::{Expr, Ident, Path, Ty},
  checker::Type,
  diag::DiagGroup,
};

mod build_graph;
mod prelude;
mod resolve_defs;
mod resolve_path;

#[derive(Debug, Default)]
pub struct Resolver {
  pub defs: Vec<Def>,
  pub diags: DiagGroup,
  pub next_use_id: UseId,
}

pub type DefId = usize;
pub type UseId = usize;

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
}

#[derive(Debug)]
enum Member {
  Child(DefId),
  ResolvedImport(DefId, UseId),
  UnresolvedImport(Option<Path>, UseId),
}

#[derive(Debug)]
pub struct ValueDef {
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
  pub generics: Vec<Ident>,
  pub adt: DefId,
  pub variant: usize,
  pub fields: Vec<Ty>,
  pub field_types: Option<Vec<Type>>,
}
