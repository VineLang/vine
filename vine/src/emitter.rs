use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

use bitflags::bitflags;

use ivy::ast::Nets;
use vine_util::bicycle::BicycleState;

use crate::{
  ast::{Builtin, Expr},
  resolve::{Def, Resolver, ValueDefKind},
};

mod build_stages;
mod finish_stages;
mod fix_interstage_wires;
mod infer_interfaces;
mod net_builder;

use net_builder::*;

pub fn emit(resolver: &Resolver, items: &[String]) -> Nets {
  let mut emitter = Emitter::new(resolver);

  if items.is_empty() {
    emitter.emit_all();
  } else {
    for def in &resolver.defs {
      let canonical = &def.canonical.to_string()[2..];
      if items.iter().any(|x| x == canonical) {
        emitter.emit_def(def);
      }
    }
  }

  emitter.nets
}

#[derive(Debug, Default)]
pub struct Emitter<'d> {
  pub nets: Nets,

  defs: &'d [Def],

  name: String,
  interfaces: Vec<Interface>,
  stages: Vec<Stage>,
  cur: Stage,
  cur_id: StageId,
  local_count: usize,

  forks: Vec<Fork>,

  net: NetBuilder,

  return_target: Option<(Local, ForkId)>,
  loop_target: Option<(Local, ForkId, StageId)>,

  dyn_fns: HashMap<usize, (Local, StageId)>,

  dup_labels: usize,

  concat: Option<String>,
}

impl<'d> Emitter<'d> {
  pub fn new(resolver: &'d Resolver) -> Self {
    let concat =
      resolver.builtins.get(&Builtin::Concat).map(|&d| resolver.defs[d].canonical.to_string());
    let defs = &resolver.defs;
    Emitter {
      nets: Default::default(),
      defs,
      name: Default::default(),
      interfaces: Default::default(),
      stages: Default::default(),
      cur: Default::default(),
      cur_id: Default::default(),
      local_count: Default::default(),
      forks: Default::default(),
      net: Default::default(),
      return_target: Default::default(),
      loop_target: Default::default(),
      dyn_fns: Default::default(),
      dup_labels: Default::default(),
      concat,
    }
  }

  pub fn emit_all(&mut self) {
    for def in self.defs {
      self.emit_def(def);
    }
  }

  pub fn emit_def(&mut self, def: &Def) {
    self.net = Default::default();
    let Some(value_def) = &def.value_def else { return };
    match &value_def.kind {
      ValueDefKind::Expr(expr) => {
        self.emit_root_expr(&mut { value_def.locals }, def.canonical.to_string(), expr, []);
      }
      ValueDefKind::Ivy(net) => {
        self.nets.insert(def.canonical.to_string(), net.clone());
      }
      ValueDefKind::AdtConstructor => {
        let net = self.emit_adt_constructor(def);
        self.nets.insert(def.canonical.to_string(), net);
      }
    }
  }

  pub(super) fn emit_root_expr(
    &mut self,
    local_count: &mut usize,
    name: String,
    expr: &Expr,
    locals: impl IntoIterator<Item = Local>,
  ) {
    let init_stage = self.build_stages(*local_count, name, expr);
    let init_interface = &mut self.interfaces[self.stages[init_stage].outer];
    for local in locals {
      let usage = Usage::GET.union(Usage::SET);
      init_interface.inward.entry(local).or_insert(usage).union_with(usage);
      init_interface.wires.insert((local, WireDir::Input));
      init_interface.wires.insert((local, WireDir::Output));
    }
    self.fix_interstage_wires();
    self.infer_interfaces();
    self.finish_stages(init_stage);
    *local_count = self.local_count;
  }

  fn new_local(&mut self) -> usize {
    let l = self.local_count;
    self.local_count += 1;
    self.declare_local(l);
    l
  }

  fn declare_local(&mut self, local: usize) {
    self.interfaces[self.cur.outer].declarations.insert(local);
  }
}

fn stage_name(base_name: &str, stage_id: StageId) -> String {
  format!("{base_name}::{stage_id}")
}

type InterfaceId = usize;
type StageId = usize;
type Local = usize;
type ForkId = usize;

#[derive(Debug, Default)]
struct Interface {
  outward: BTreeMap<Local, Usage>,
  inward: BTreeMap<Local, Usage>,
  wires: BTreeSet<(Local, WireDir)>,
  declarations: BTreeSet<Local>,
  stages: Vec<StageId>,
  parents: BTreeSet<InterfaceId>,
  state: BicycleState,
}

#[derive(Debug, Default)]
struct Stage {
  outer: InterfaceId,
  agents: Vec<Agent>,
  steps: VecDeque<Step>,
  fin: Vec<Step>,
  divergence: ForkId,
  header: Option<(Port, Port)>,
}

#[derive(Debug)]
struct Fork {
  ends: Vec<StageId>,
  divergence: ForkId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum WireDir {
  Input,
  Output,
}

#[derive(Debug)]
enum Step {
  Get(Local, Port),
  Set(Local, Port),
  Move(Local, Port),
  Call(InterfaceId, Port),
}

impl Step {
  fn port_mut(&mut self) -> &mut Port {
    match self {
      Step::Get(_, p) | Step::Set(_, p) | Step::Move(_, p) | Step::Call(_, p) => p,
    }
  }
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
    assert_eq!(Usage::join(a, a), a);

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

        assert_eq!(
          [a, b, a, c, a].into_iter().reduce(Usage::join),
          [a, b, c, a].into_iter().reduce(Usage::join),
        );
      }
    }
  }

  fn all() -> impl Iterator<Item = Usage> {
    (0..16).map(Usage::from_bits_truncate)
  }
}
