use std::collections::BTreeMap;

use ivy::ast::Net;
use vine_util::{
  idx::{Counter, IdxVec},
  multi_iter, new_idx,
};

use crate::{
  analyzer::{usage::Usage, UsageVar},
  chart::{EnumId, ValueDefId, VariantId},
  diag::ErrorGuaranteed,
  tir::{Local, TirImpl},
};

new_idx!(pub LayerId; n => ["L{n}"]);
new_idx!(pub StageId; n => ["s{n}"]);
new_idx!(pub InterfaceId; n => ["i{n}"]);
new_idx!(pub WireId; n => ["w{n}"]);

impl LayerId {
  pub const NONE: LayerId = LayerId(usize::MAX);
}

#[derive(Debug, Clone)]
pub struct VIR {
  pub locals: Counter<Local>,
  pub layers: IdxVec<LayerId, Layer>,
  pub interfaces: IdxVec<InterfaceId, Interface>,
  pub stages: IdxVec<StageId, Stage>,
  pub globals: Vec<(Local, Usage)>,
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
  pub interior: Option<UsageVar>,
  pub exterior: Option<UsageVar>,
  pub wires: BTreeMap<Local, (Usage, Usage)>,
}

impl Interface {
  pub fn new(id: InterfaceId, layer: LayerId, kind: InterfaceKind) -> Self {
    Interface {
      id,
      layer,
      kind,
      incoming: 0,
      interior: None,
      exterior: None,
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
  Branch(StageId, StageId),
  Match(EnumId, IdxVec<VariantId, StageId>),
  Closure { call: StageId, fork: Option<StageId>, drop: Option<StageId> },
}

impl InterfaceKind {
  pub fn stage_count(&self) -> usize {
    match self {
      InterfaceKind::Unconditional(_) => 1,
      InterfaceKind::Branch(..) => 2,
      InterfaceKind::Match(_, stages) => stages.len(),
      InterfaceKind::Closure { fork, drop, .. } => {
        1 + (fork.is_some() as usize) + (drop.is_some() as usize)
      }
    }
  }
  pub fn stage(&self, i: usize) -> StageId {
    match self {
      InterfaceKind::Unconditional(s) => *s,
      InterfaceKind::Branch(a, b) => [*a, *b][i],
      InterfaceKind::Match(_, s) => s[VariantId(i)],
      InterfaceKind::Closure { call, fork, drop } => match i {
        0 => *call,
        1 => fork.or(*drop).unwrap(),
        _ => drop.unwrap(),
      },
    }
  }
}

#[derive(Debug, Clone)]
pub struct Stage {
  pub id: StageId,
  pub interface: InterfaceId,
  pub layer: LayerId,
  pub header: Header,
  pub declarations: Vec<Local>,
  pub steps: Vec<Step>,
  pub transfer: Option<Transfer>,
  pub wires: Counter<WireId>,
}

#[derive(Default, Debug, Clone)]
pub enum Header {
  #[default]
  None,
  Match(Option<Port>),
  Fn(Vec<Port>, Port),
  Fork(Port, Port),
  Drop,
}

impl Header {
  pub fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Zero, Two, Opt, Fn });
    match self {
      Header::None | Header::Drop => Ports::Zero([]),
      Header::Fork(a, b) => Ports::Two([a, b]),
      Header::Match(a) => Ports::Opt(a),
      Header::Fn(a, b) => Ports::Fn(a.iter().chain([b])),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Step {
  Invoke(Local, Invocation),
  Transfer(Transfer),
  Diverge(LayerId, Option<Transfer>),

  Link(Port, Port),

  Call(TirImpl, Port, Vec<Port>, Port),
  Composite(Port, Vec<Port>),
  Enum(Port, EnumId, VariantId, Option<Port>),
  Ref(Port, Port, Port),
  ExtFn(&'static str, bool, Port, Port, Port),
  Dup(Port, Port, Port),
  List(Port, Vec<Port>),
  String(Port, String, Vec<(Port, String)>),
  InlineIvy(Vec<(String, Port)>, Port, Net),
}

impl Step {
  pub fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Zero, Two, Three, Invoke, Transfer, Composite, Enum, Fn, String, Ivy });
    match self {
      Step::Invoke(_, invocation) => Ports::Invoke(invocation.ports()),
      Step::Transfer(transfer) | Step::Diverge(_, Some(transfer)) => {
        Ports::Transfer(transfer.data.as_ref())
      }
      Step::Diverge(_, None) => Ports::Zero([]),
      Step::Link(a, b) => Ports::Two([a, b]),
      Step::Call(_, f, a, r) => Ports::Fn([f].into_iter().chain(a).chain([r])),
      Step::Composite(port, ports) | Step::List(port, ports) => {
        Ports::Composite([port].into_iter().chain(ports))
      }
      Step::Enum(port, _, _, data) => Ports::Enum([port].into_iter().chain(data)),
      Step::Ref(a, b, c) | Step::ExtFn(_, _, a, b, c) | Step::Dup(a, b, c) => {
        Ports::Three([a, b, c])
      }
      Step::String(a, _, b) => Ports::String([a].into_iter().chain(b.iter().map(|x| &x.0))),
      Step::InlineIvy(binds, root, _) => Ports::Ivy(binds.iter().map(|x| &x.1).chain([root])),
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
  Def(ValueDefId),
  // Rel(RelId),
  N32(u32),
  F32(f32),
  Wire(WireId),
  Error(ErrorGuaranteed),
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
    (Port::Wire(w), Port::Wire(w))
  }

  pub fn get_local_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::Get(to)));
  }

  pub fn hedge_local_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::Hedge(to)));
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

  pub fn hedge_local(&mut self, local: Local) -> Port {
    let wire = self.new_wire();
    self.hedge_local_to(local, wire.0);
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

  pub fn erase_local(&mut self, local: Local) {
    self.steps.push(Step::Invoke(local, Invocation::Erase));
  }

  pub fn dup(&mut self, p: Port) -> (Port, Port) {
    let a = self.new_wire();
    let b = self.new_wire();
    self.steps.push(Step::Dup(p, a.0, b.0));
    (a.1, b.1)
  }

  pub fn ext_fn(&mut self, ext_fn: &'static str, swap: bool, lhs: Port, rhs: Port) -> Port {
    let out = self.new_wire();
    self.steps.push(Step::ExtFn(ext_fn, swap, lhs, rhs, out.0));
    out.1
  }

  pub fn ref_place(&mut self, (value, space): (Port, Port)) -> Port {
    let wire = self.new_wire();
    self.steps.push(Step::Ref(wire.0, value, space));
    wire.1
  }
}
