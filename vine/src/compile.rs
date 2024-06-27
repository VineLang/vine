use std::collections::{BTreeMap, BTreeSet};

use bitflags::bitflags;

use ivy::ast::{Nets, Tree};
use vine_util::bicycle::BicycleState;

use crate::resolve::{Node, NodeValue};

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
    let Some(value) = &node.value else { return };
    match value {
      NodeValue::Term(term) => {
        let init = self.build_stages(node, term);
        self.infer_interfaces();
        self.finish_nets(init);
      }
      NodeValue::Ivy(net) => {
        self.nets.insert(node.canonical.to_string(), net.clone());
      }
    }
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
  fin: Vec<Step>,
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
  Move(Local, Tree),
  Call(InterfaceId, Tree),
}

bitflags! {
  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  struct Usage: u8 {
    const MAY_GET = 1;
    const MAY_SET = 2;
    const MAY_NOT_SET = 4;
    const MAY_SET_SOME = 8;
  }
}

impl Usage {
  const NONE: Usage = Usage::MAY_NOT_SET;
  const GET: Usage = Usage::MAY_GET.union(Usage::MAY_NOT_SET);
  const SET: Usage = Usage::MAY_SET.union(Usage::MAY_SET_SOME);
  const MOVE: Usage = Usage::MAY_GET.union(Usage::MAY_SET);
  const ERASE: Usage = Usage::MAY_SET;

  fn may_get(&self) -> bool {
    self.contains(Usage::MAY_GET)
  }

  fn may_set(&self) -> bool {
    self.contains(Usage::MAY_SET)
  }

  fn may_not_set(&self) -> bool {
    self.contains(Usage::MAY_NOT_SET)
  }

  fn may_set_some(&self) -> bool {
    self.contains(Usage::MAY_SET_SOME)
  }

  fn new(f: impl FnOnce(&mut Self)) -> Self {
    let mut x = Self::empty();
    f(&mut x);
    x
  }

  fn join(a: Self, b: Self) -> Self {
    Usage::new(|u| {
      u.set(Usage::MAY_GET, a.may_get() | (a.may_not_set() & b.may_get()));
      u.set(Usage::MAY_SET, a.may_set() | b.may_set());
      u.set(Usage::MAY_NOT_SET, a.may_not_set() & b.may_not_set());
      u.set(Usage::MAY_SET_SOME, b.may_set_some() | (b.may_not_set() & a.may_set_some()));
    })
  }

  fn forward(self) -> Self {
    self & (Usage::MAY_SET | Usage::MAY_SET_SOME)
  }

  fn backward(self) -> Self {
    self & Usage::GET
  }

  fn append(&mut self, with: Usage) {
    *self = Usage::join(*self, with)
  }

  fn prepend(&mut self, with: Usage) {
    *self = Usage::join(with, *self)
  }

  fn union_with(&mut self, with: Usage) {
    *self = Usage::union(*self, with)
  }
}

#[test]
fn usage_axioms() {
  for a in all() {
    assert_eq!(Usage::join(a, Usage::NONE), a);
    assert_eq!(Usage::join(Usage::NONE, a), a);

    assert_eq!(Usage::union(a, a), a);

    for b in all() {
      assert_eq!(Usage::union(a, b), Usage::union(b, a));

      for c in all() {
        assert_eq!(Usage::union(Usage::union(a, b), c), Usage::union(a, Usage::union(b, c)),);
        assert_eq!(Usage::join(Usage::join(a, b), c), Usage::join(a, Usage::join(b, c)));

        assert_eq!(
          Usage::join(a, Usage::union(b, c)),
          Usage::union(Usage::join(a, b), Usage::join(a, c))
        );
        assert_eq!(
          Usage::join(Usage::union(a, b), c),
          Usage::union(Usage::join(a, c), Usage::join(b, c))
        );
      }
    }
  }

  fn all() -> impl Iterator<Item = Usage> {
    (0..16).map(Usage::from_bits_truncate)
  }
}
