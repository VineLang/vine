use ivm::ext::ExtFn;
use vine_util::{
  idx::{Counter, IdxVec},
  new_idx,
};

use crate::{ast::Local, resolver::DefId};

new_idx!(pub LayerId);
new_idx!(pub StageId);
new_idx!(pub InterfaceId);
new_idx!(pub WireId);

pub struct VIR {
  pub layers: IdxVec<LayerId, Option<Layer>>,
  pub interfaces: IdxVec<InterfaceId, Option<Interface>>,
  pub stages: IdxVec<StageId, Option<Stage>>,
}

pub struct Layer {
  pub id: LayerId,
  pub parent: Option<LayerId>,
  pub stages: Vec<StageId>,
}

pub struct Interface {
  pub id: InterfaceId,
  pub kind: InterfaceKind,
}

pub enum InterfaceKind {
  Unconditional(StageId),
  Branch(StageId, StageId),
  Match(DefId, Vec<StageId>),
}

pub struct Stage {
  pub id: StageId,
  pub interface: InterfaceId,
  pub layer: LayerId,
  pub steps: Vec<Step>,
  pub transfer: Option<Transfer>,
  pub wire: Counter<WireId>,
}

pub enum Step {
  Local(Local, LocalUse),
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

pub enum LocalUse {
  Erase,
  Get(Port),
  Hedge(Port),
  Take(Port),
  Set(Port),
  Mut(Port, Port),
}

pub enum Port {
  Erase,
  Const(DefId),
  N32(u32),
  F32(f32),
  Wire(WireId),
}

pub struct Transfer {
  pub interface: InterfaceId,
  pub data: Option<Port>,
}

impl Stage {
  pub fn new_wire(&mut self) -> (Port, Port) {
    let w = self.wire.next();
    (Port::Wire(w), Port::Wire(w))
  }

  pub fn erase(&mut self, port: Port) {
    if matches!(port, Port::Wire(_)) {
      self.steps.push(Step::Link(port, Port::Erase));
    }
  }

  pub fn get_local_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Local(local, LocalUse::Get(to)));
  }

  pub fn take_local_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Local(local, LocalUse::Take(to)));
  }

  pub fn set_local_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Local(local, LocalUse::Set(to)));
  }

  pub fn mut_local_to(&mut self, local: Local, o: Port, i: Port) {
    self.steps.push(Step::Local(local, LocalUse::Mut(o, i)));
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
