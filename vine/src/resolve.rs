use std::collections::HashMap;

use crate::ast::{Ident, Path, Term};

mod build_graph;
mod resolve_path;
mod resolve_terms;

#[derive(Debug, Default)]
pub struct Resolver {
  pub nodes: Vec<Node>,
}

pub type NodeId = usize;

#[derive(Debug)]
pub struct Node {
  pub id: NodeId,
  pub canonical: Path,
  pub value: Option<Term>,

  children: HashMap<Ident, NodeId>,
  imports: HashMap<Ident, Option<Path>>,
  parent: Option<NodeId>,
}
