use std::collections::BTreeMap;

use ivy::ast::Net;
use vine_util::{
  idx::{Counter, IdxVec},
  multi_iter, new_idx,
};

use crate::{
  components::analyzer::{usage::Usage, UsageVar},
  structures::{
    ast::Span,
    chart::{EnumId, StructId, VariantId},
    diag::ErrorGuaranteed,
    resolutions::{ConstRelId, FnRelId, Rels},
    tir::{ClosureId, Local, LocalInfo},
    types::{Type, Types},
  },
};

new_idx!(pub LayerId; n => ["L{n}"]);
new_idx!(pub StageId; n => ["s{n}"]);
new_idx!(pub InterfaceId; n => ["i{n}"]);
new_idx!(pub WireId; n => ["w{n}"]);

impl LayerId {
  pub const NONE: LayerId = LayerId(usize::MAX);
}

#[derive(Debug, Clone)]
pub struct Vir<'core> {
  pub types: Types<'core>,
  pub locals: IdxVec<Local, LocalInfo>,
  pub rels: Rels,
  pub layers: IdxVec<LayerId, Layer>,
  pub interfaces: IdxVec<InterfaceId, Interface>,
  pub stages: IdxVec<StageId, Stage>,
  pub globals: Vec<(Local, Usage)>,
  pub closures: IdxVec<ClosureId, InterfaceId>,
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
  Match(EnumId, Vec<StageId>),
  Fn { call: StageId, fork: Option<StageId>, drop: Option<StageId> },
}

impl InterfaceKind {
  pub fn stages(&self) -> impl Iterator<Item = StageId> {
    multi_iter! { Stages { One, Two, Vec, Fn } }
    match self {
      InterfaceKind::Unconditional(a) => Stages::One([*a]),
      InterfaceKind::Branch(a, b) => Stages::Two([*a, *b]),
      InterfaceKind::Match(_, s) => Stages::Vec(s.clone()),
      InterfaceKind::Fn { call, fork, drop } => {
        Stages::Fn([*call].into_iter().chain(*fork).chain(*drop))
      }
    }
  }
}

#[derive(Debug, Clone)]
pub struct Stage {
  pub span: Span,
  pub id: StageId,
  pub interface: InterfaceId,
  pub layer: LayerId,
  pub header: Header,
  pub declarations: Vec<Local>,
  pub steps: Vec<Step>,
  pub transfer: Option<Transfer>,
  pub wires: Counter<WireId>,
}

#[derive(Debug, Clone)]
pub enum Header {
  None,
  Match(Option<Port>),
  Fn(Vec<Port>, Port),
  Fork(Port, Port),
  Drop,
}

#[derive(Debug, Clone)]
pub enum Step {
  Invoke(Local, Invocation),
  Transfer(Transfer),
  Diverge(LayerId, Option<Transfer>),

  Link(Port, Port),

  Call(FnRelId, Option<Port>, Vec<Port>, Port),
  Composite(Port, Vec<Port>),
  Struct(StructId, Port, Port),
  Enum(EnumId, VariantId, Port, Option<Port>),
  Ref(Port, Port, Port),
  ExtFn(&'static str, bool, Port, Port, Port),
  Dup(Port, Port, Port),
  List(Port, Vec<Port>),
  String(Port, String, Vec<(Port, String)>),
  InlineIvy(Vec<(String, Port)>, Port, Net),
}

impl Header {
  pub fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Zero, Two, Opt, Fn });
    match self {
      Header::None | Header::Drop => Ports::Zero([]),
      Header::Match(a) => Ports::Opt(a),
      Header::Fn(params, ret) => Ports::Fn(params.iter().chain([ret])),
      Header::Fork(a, b) => Ports::Two([a, b]),
    }
  }
}

impl Step {
  pub fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Zero, One, Two, Three, Invoke, Transfer, Tuple, Fn, String, Ivy });
    match self {
      Step::Invoke(_, invocation) => Ports::Invoke(invocation.ports()),
      Step::Transfer(transfer) | Step::Diverge(_, Some(transfer)) => {
        Ports::Transfer(transfer.data.as_ref())
      }
      Step::Diverge(_, None) => Ports::Zero([]),
      Step::Enum(_, _, a, None) => Ports::One([a]),
      Step::Link(a, b) | Step::Struct(_, a, b) | Step::Enum(_, _, a, Some(b)) => Ports::Two([a, b]),
      Step::Call(_, f, a, r) => Ports::Fn(f.as_ref().into_iter().chain(a).chain([r])),
      Step::Composite(port, ports) | Step::List(port, ports) => {
        Ports::Tuple([port].into_iter().chain(ports))
      }
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
pub struct Port {
  pub ty: Type,
  pub kind: PortKind,
}

impl Port {
  pub fn error(ty: Type, err: ErrorGuaranteed) -> Port {
    Port { ty, kind: PortKind::Error(err) }
  }
}

#[derive(Debug, Clone)]
pub enum PortKind {
  Nil,
  ConstRel(ConstRelId),
  N32(u32),
  F32(f32),
  Wire(Span, WireId),
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

pub struct Wire {
  pub pos: Port,
  pub neg: Port,
}

impl Stage {
  pub fn new_wire(&mut self, span: Span, ty: Type) -> Wire {
    let w = self.wires.next();
    Wire {
      pos: Port { ty, kind: PortKind::Wire(span, w) },
      neg: Port { ty: ty.inverse(), kind: PortKind::Wire(span, w) },
    }
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

  pub fn get_local(&mut self, local: Local, span: Span, ty: Type) -> Port {
    let wire = self.new_wire(span, ty);
    self.get_local_to(local, wire.neg);
    wire.pos
  }

  pub fn hedge_local(&mut self, local: Local, span: Span, ty: Type) -> Port {
    let wire = self.new_wire(span, ty);
    self.get_local_to(local, wire.pos);
    wire.neg
  }

  pub fn take_local(&mut self, local: Local, span: Span, ty: Type) -> Port {
    let wire = self.new_wire(span, ty);
    self.take_local_to(local, wire.neg);
    wire.pos
  }

  pub fn set_local(&mut self, local: Local, span: Span, ty: Type) -> Port {
    let wire = self.new_wire(span, ty);
    self.set_local_to(local, wire.pos);
    wire.neg
  }

  pub fn mut_local(&mut self, local: Local, span: Span, ty: Type) -> (Port, Port) {
    let o = self.new_wire(span, ty);
    let i = self.new_wire(span, ty);
    self.mut_local_to(local, o.neg, i.pos);
    (o.pos, i.neg)
  }

  pub fn erase_local(&mut self, local: Local) {
    self.steps.push(Step::Invoke(local, Invocation::Erase));
  }

  pub fn dup(&mut self, span: Span, p: Port) -> (Port, Port) {
    let a = self.new_wire(span, p.ty);
    let b = self.new_wire(span, p.ty);
    self.steps.push(Step::Dup(p, a.neg, b.neg));
    (a.pos, b.pos)
  }

  pub fn ext_fn(
    &mut self,
    span: Span,
    ext_fn: &'static str,
    swap: bool,
    lhs: Port,
    rhs: Port,
    ty: Type,
  ) -> Port {
    let out = self.new_wire(span, ty);
    self.steps.push(Step::ExtFn(ext_fn, swap, lhs, rhs, out.neg));
    out.pos
  }

  pub fn ref_place(&mut self, span: Span, ty: Type, (value, space): (Port, Port)) -> Port {
    let wire = self.new_wire(span, ty);
    self.steps.push(Step::Ref(wire.neg, value, space));
    wire.pos
  }
}
