use std::collections::HashMap;

use ivy::ast::Net;

use crate::{
  ast::{Ident, Path, Term},
  diag::DiagGroup,
};

mod build_graph;
mod resolve_path;
mod resolve_terms;

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
pub enum NodeValue {
  Term(Term),
  Ivy(Net),
  AdtConstructor,
}

#[derive(Debug)]
pub struct Adt {
  pub variants: Vec<NodeId>,
}

#[derive(Debug)]
pub struct Variant {
  pub adt: NodeId,
  pub variant: usize,
  pub fields: Vec<Ident>,
}
