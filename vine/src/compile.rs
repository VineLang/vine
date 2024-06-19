use std::collections::{BTreeMap, BTreeSet};

use ivy::ast::{Nets, Tree};
use vine_util::bicycle::BicycleState;

use crate::resolve::Node;

mod build_stages;
mod finish_nets;
mod infer_interfaces;

#[derive(Debug, Default)]
pub struct Compiler {
  pub nets: Nets,

  name: String,
  interfaces: Vec<Interface>,
  stages: Vec<Stage>,
  cur: Stage,
  cur_id: StageId,
  local_count: usize,
}

impl Compiler {
  pub fn compile_node(&mut self, node: &Node) {
    let Some(term) = &node.value else { return };
    let init = self.build_stages(node, term);
    self.infer_interfaces();
    self.finish_nets(init);
  }
}

fn stage_name(base_name: &str, stage_id: StageId) -> String {
  format!("{base_name}::{stage_id}")
}

#[derive(Debug, Default)]
struct VarGen(usize);

impl VarGen {
  fn gen(&mut self) -> (Tree, Tree) {
    let n = self.0;
    self.0 += 1;
    let str = format!("n{n}");
    (Tree::Var(str.clone()), Tree::Var(str))
  }
}

type InterfaceId = usize;
type StageId = usize;
type Local = usize;

#[derive(Debug, Default)]
struct Interface {
  outward: BTreeMap<Local, Usage>,
  inward: BTreeMap<Local, Usage>,
  wires: BTreeSet<(Local, WireDir)>,
  stages: Vec<StageId>,
  parents: BTreeSet<InterfaceId>,
  state: BicycleState,
}

#[derive(Debug, Default)]
struct Stage {
  outer: InterfaceId,
  pairs: Vec<(Tree, Tree)>,
  steps: Vec<Step>,
  var: VarGen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum WireDir {
  Input,
  Output,
}

#[derive(Debug)]
enum Step {
  Get(Local, Tree),
  Set(Local, Tree),
  Call(InterfaceId, Tree),
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Usage {
  #[default]
  None,
  Get,
  Set,
  GetSet,
}

impl Usage {
  fn join(a: Usage, b: Usage) -> Usage {
    match (a, b) {
      (x, Usage::None) => x,
      (Usage::None, x) => x,
      (Usage::Set | Usage::GetSet, _) => a,
      (Usage::Get, Usage::Set | Usage::GetSet) => Usage::GetSet,
      (Usage::Get, Usage::Get) => Usage::Get,
    }
  }

  fn forward(self) -> Self {
    match self {
      Usage::None | Usage::Get => Usage::None,
      Usage::Set | Usage::GetSet => Usage::Set,
    }
  }

  fn backward(self) -> Self {
    match self {
      Usage::None | Usage::Set => Usage::None,
      Usage::Get | Usage::GetSet => Usage::Get,
    }
  }

  fn append(&mut self, with: Usage) {
    *self = Usage::join(*self, with)
  }

  fn prepend(&mut self, with: Usage) {
    *self = Usage::join(with, *self)
  }

  fn union(&mut self, with: Usage) {
    *self = match (*self, with) {
      (x, Usage::None) => x,
      (Usage::None, x) => x,
      (Usage::Get, Usage::Get) => Usage::Get,
      (Usage::Set, Usage::Set) => Usage::Set,
      (_, _) => Usage::GetSet,
    }
  }
}
