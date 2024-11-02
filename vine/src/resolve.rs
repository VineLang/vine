use std::collections::HashMap;

use ivy::ast::Net;

use crate::{
  ast::{Expr, Ident, Path, Ty},
  checker::Type,
  diag::DiagGroup,
};

mod build_graph;
mod prelude;
mod resolve_items;
mod resolve_path;

#[derive(Debug, Default)]
pub struct Resolver {
  pub nodes: Vec<Node>,
  pub diags: DiagGroup,
  pub next_use_id: UseId,
}

pub type NodeId = usize;
pub type UseId = usize;

#[derive(Debug)]
pub struct Node {
  pub id: NodeId,
  pub canonical: Path,

  pub locals: usize,
  pub value: Option<NodeValue>,

  pub typ: Option<NodeType>,
  pub adt: Option<Adt>,
  pub variant: Option<Variant>,

  members: HashMap<Ident, Member>,
  parent: Option<NodeId>,
}

#[derive(Debug)]
enum Member {
  Child(NodeId),
  ResolvedImport(NodeId, UseId),
  UnresolvedImport(Option<Path>, UseId),
}

#[derive(Debug)]
pub struct NodeValue {
  pub generics: Vec<Ident>,
  pub annotation: Option<Ty>,
  pub ty: Option<Type>,
  pub kind: NodeValueKind,
}

#[derive(Debug)]
pub struct NodeType {
  pub generics: Vec<Ident>,
  pub alias: Option<Ty>,
  pub ty: Option<Type>,
}

#[derive(Debug)]
pub enum NodeValueKind {
  Expr(Expr),
  Ivy(Net),
  AdtConstructor,
}

#[derive(Debug)]
pub struct Adt {
  pub generics: Vec<Ident>,
  pub variants: Vec<NodeId>,
}

#[derive(Debug)]
pub struct Variant {
  pub generics: Vec<Ident>,
  pub adt: NodeId,
  pub variant: usize,
  pub fields: Vec<Ty>,
  pub field_tys: Option<Vec<Type>>,
}
