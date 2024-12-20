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

  pub fn erase(&self, port: Port) {
    todo!()
  }
}
