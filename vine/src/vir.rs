use core::slice;
use std::collections::BTreeMap;

use ivm::ext::ExtFn;
use vine_util::{
  idx::{Counter, IdxVec},
  multi_iter, new_idx,
};

use crate::{
  analyzer::{usage::Usage, Usages},
  ast::Local,
  resolver::DefId,
};

new_idx!(pub LayerId);
new_idx!(pub StageId);
new_idx!(pub InterfaceId);
new_idx!(pub WireId);

impl LayerId {
  pub const NONE: LayerId = LayerId(usize::MAX);
}

#[derive(Debug, Clone)]
pub struct VIR {
  pub locals: Counter<Local>,
  pub layers: IdxVec<LayerId, Layer>,
  pub interfaces: IdxVec<InterfaceId, Interface>,
  pub stages: IdxVec<StageId, Stage>,
}

#[derive(Debug, Clone)]
pub struct Layer {
  pub id: LayerId,
  pub parent: Option<LayerId>,
  pub stages: Vec<StageId>,
}

#[derive(Debug, Clone)]
pub struct Interface {
  pub id: InterfaceId,
  pub layer: LayerId,
  pub kind: InterfaceKind,

  pub incoming: usize,
  pub parents: Vec<InterfaceId>,
  pub interior_canonicity: usize,
  pub interior: Usages,
  pub exterior_canonicity: usize,
  pub exterior: Usages,
  pub wires: BTreeMap<Local, (Usage, Usage)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum WireDir {
  Input,
  Output,
}

impl Interface {
  pub fn new(id: InterfaceId, layer: LayerId, kind: InterfaceKind) -> Self {
    Interface {
      id,
      layer,
      kind,
      incoming: 0,
      parents: Vec::new(),
      exterior_canonicity: 0,
      exterior: Usages::default(),
      interior_canonicity: 0,
      interior: Usages::default(),
      wires: BTreeMap::default(),
    }
  }

  pub fn inline(&self) -> bool {
    self.id != InterfaceId(0)
      && self.incoming == 1
      && matches!(self.kind, InterfaceKind::Unconditional(_))
  }
}

#[derive(Debug, Clone)]
pub enum InterfaceKind {
  Unconditional(StageId),
  Branch([StageId; 2]),
  Match(DefId, Vec<StageId>),
}

impl InterfaceKind {
  pub fn stages(&self) -> &[StageId] {
    match self {
      InterfaceKind::Unconditional(s) => slice::from_ref(s),
      InterfaceKind::Branch(s) => s,
      InterfaceKind::Match(_, s) => s,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Stage {
  pub id: StageId,
  pub interface: InterfaceId,
  pub layer: LayerId,
  pub header: Vec<Port>,
  pub declarations: Vec<Local>,
  pub steps: Vec<Step>,
  pub transfer: Option<Transfer>,
  pub wires: Counter<WireId>,
}

#[derive(Debug, Clone)]
pub enum Step {
  Invoke(Local, Invocation),
  Transfer(Transfer),
  Diverge(LayerId, Option<Transfer>),

  Link(Port, Port),

  Fn(Port, Vec<Port>, Port),
  Tuple(Port, Vec<Port>),
  Adt(DefId, Port, Vec<Port>),
  Ref(Port, Port, Port),
  ExtFn(ExtFn, Port, Port, Port),
  Dup(Port, Port, Port),
  List(Port, Vec<Port>),
  String(Port, String),
}

impl Step {
  pub fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Zero, One, Two, Three, Invoke, Transfer, Tuple, Fn });
    match self {
      Step::Invoke(_, invocation) => Ports::Invoke(invocation.ports()),
      Step::Transfer(transfer) | Step::Diverge(_, Some(transfer)) => {
        Ports::Transfer(transfer.data.as_ref())
      }
      Step::Diverge(_, None) => Ports::Zero([]),
      Step::Link(a, b) => Ports::Two([a, b]),
      Step::Fn(f, a, r) => Ports::Fn([f].into_iter().chain(a).chain([r])),
      Step::Tuple(port, ports) | Step::List(port, ports) | Step::Adt(_, port, ports) => {
        Ports::Tuple([port].into_iter().chain(ports))
      }
      Step::Ref(a, b, c) | Step::ExtFn(_, a, b, c) | Step::Dup(a, b, c) => Ports::Three([a, b, c]),
      Step::String(port, _) => Ports::One([port]),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Invocation {
  Erase,
  Get(Port),
  Hedge(Port),
  Take(Port),
  Set(Port),
  Mut(Port, Port),
}

impl Invocation {
  fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Zero, One, Two });
    match self {
      Invocation::Erase => Ports::Zero([]),
      Invocation::Get(a) | Invocation::Hedge(a) | Invocation::Take(a) | Invocation::Set(a) => {
        Ports::One([a])
      }
      Invocation::Mut(a, b) => Ports::Two([a, b]),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Port {
  Erase,
  Const(DefId),
  N32(u32),
  F32(f32),
  Wire(WireId, StageId),
}

#[derive(Debug, Clone)]
pub struct Transfer {
  pub interface: InterfaceId,
  pub data: Option<Port>,
}

impl Transfer {
  pub fn unconditional(interface: InterfaceId) -> Transfer {
    Transfer { interface, data: None }
  }
}

impl Stage {
  pub fn new_wire(&mut self) -> (Port, Port) {
    let w = self.wires.next();
    (Port::Wire(w, self.id), Port::Wire(w, self.id))
  }

  pub fn erase(&mut self, port: Port) {
    if matches!(port, Port::Wire(..)) {
      self.steps.push(Step::Link(port, Port::Erase));
    }
  }

  pub fn get_local_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::Get(to)));
  }

  pub fn take_local_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::Take(to)));
  }

  pub fn set_local_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::Set(to)));
  }

  pub fn mut_local_to(&mut self, local: Local, o: Port, i: Port) {
    self.steps.push(Step::Invoke(local, Invocation::Mut(o, i)));
  }

  pub fn get_local(&mut self, local: Local) -> Port {
    let wire = self.new_wire();
    self.get_local_to(local, wire.0);
    wire.1
  }

  pub fn take_local(&mut self, local: Local) -> Port {
    let wire = self.new_wire();
    self.take_local_to(local, wire.0);
    wire.1
  }

  pub fn set_local(&mut self, local: Local) -> Port {
    let wire = self.new_wire();
    self.set_local_to(local, wire.0);
    wire.1
  }

  pub fn mut_local(&mut self, local: Local) -> (Port, Port) {
    let o = self.new_wire();
    let i = self.new_wire();
    self.mut_local_to(local, o.0, i.0);
    (o.1, i.1)
  }

  pub fn dup(&mut self, p: Port) -> (Port, Port) {
    let a = self.new_wire();
    let b = self.new_wire();
    self.steps.push(Step::Dup(p, a.0, b.0));
    (a.1, b.1)
  }

  pub fn ext_fn(&mut self, ext_fn: ExtFn, lhs: Port, rhs: Port) -> Port {
    let out = self.new_wire();
    self.steps.push(Step::ExtFn(ext_fn, lhs, rhs, out.0));
    out.1
  }
}
