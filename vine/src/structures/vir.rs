use std::collections::BTreeMap;

use ivy::ast::Net;
use vine_util::{
  idx::{Counter, IdxVec},
  multi_iter, new_idx,
};

use crate::{
  components::{analyzer::EffectVar, finder::Finder},
  structures::{
    ast::Span,
    chart::{Chart, DefId, EnumId, GenericsId, StructId, VariantId},
    core::Core,
    diag::ErrorGuaranteed,
    resolutions::{ConstRelId, FnRelId, Rels},
    signatures::Signatures,
    tir::{ClosureId, Local},
    types::{Inverted, Type, Types},
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
  pub locals: IdxVec<Local, VirLocal>,
  pub rels: Rels,
  pub layers: IdxVec<LayerId, Layer>,
  pub interfaces: IdxVec<InterfaceId, Interface>,
  pub stages: IdxVec<StageId, Stage>,
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
  pub interior: Option<EffectVar>,
  pub exterior: Option<EffectVar>,
  pub wires: BTreeMap<Local, (bool, bool, bool)>,
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
  Inspect(Vec<Local>),
}

impl InterfaceKind {
  pub fn stages(&self) -> impl Iterator<Item = StageId> {
    multi_iter! { Stages { Zero, One, Two, Vec, Fn } }
    match self {
      InterfaceKind::Unconditional(a) => Stages::One([*a]),
      InterfaceKind::Branch(a, b) => Stages::Two([*a, *b]),
      InterfaceKind::Match(_, s) => Stages::Vec(s.clone()),
      InterfaceKind::Fn { call, fork, drop } => {
        Stages::Fn([*call].into_iter().chain(*fork).chain(*drop))
      }
      InterfaceKind::Inspect(_) => Stages::Zero([]),
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

#[derive(Default, Debug, Clone)]
pub enum Header {
  #[default]
  None,
  Match(Option<Port>),
  Fn(Vec<Port>, Port),
  Fork(Port, Port),
  Drop,
  Entry(Vec<Port>),
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
  List(Port, Vec<Port>),
  String(Port, String, Vec<(Port, String)>),
  InlineIvy(Vec<(String, Port)>, Port, Net),
}

impl Header {
  pub fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Zero, Two, Opt, Fn, Vec });
    match self {
      Header::None | Header::Drop => Ports::Zero([]),
      Header::Match(a) => Ports::Opt(a),
      Header::Fn(params, ret) => Ports::Fn(params.iter().chain([ret])),
      Header::Fork(a, b) => Ports::Two([a, b]),
      Header::Entry(ports) => Ports::Vec(ports),
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
      Step::Ref(a, b, c) | Step::ExtFn(_, _, a, b, c) => Ports::Three([a, b, c]),
      Step::String(a, _, b) => Ports::String([a].into_iter().chain(b.iter().map(|x| &x.0))),
      Step::InlineIvy(binds, root, _) => Ports::Ivy(binds.iter().map(|x| &x.1).chain([root])),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Invocation {
  Bar,
  Read(Port),
  Write(Port),
  ReadBar(Port),
  BarWrite(Port),
  ReadWrite(Port, Port),
}

impl Invocation {
  fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Zero, One, Two });
    match self {
      Invocation::Bar => Ports::Zero([]),
      Invocation::Read(a)
      | Invocation::Write(a)
      | Invocation::ReadBar(a)
      | Invocation::BarWrite(a) => Ports::One([a]),
      Invocation::ReadWrite(a, b) => Ports::Two([a, b]),
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

  pub fn local_read_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::Read(to)));
  }

  pub fn local_write_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::Write(to)));
  }

  pub fn local_read_bar_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::ReadBar(to)));
  }

  pub fn local_bar_write_to(&mut self, local: Local, to: Port) {
    self.steps.push(Step::Invoke(local, Invocation::BarWrite(to)));
  }

  pub fn local_read_write_to(&mut self, local: Local, o: Port, i: Port) {
    self.steps.push(Step::Invoke(local, Invocation::ReadWrite(o, i)));
  }

  pub fn local_read(&mut self, local: Local, span: Span, ty: Type) -> Port {
    let wire = self.new_wire(span, ty);
    self.local_read_to(local, wire.neg);
    wire.pos
  }

  pub fn local_write(&mut self, local: Local, span: Span, ty: Type) -> Port {
    let wire = self.new_wire(span, ty);
    self.local_write_to(local, wire.pos);
    wire.neg
  }

  pub fn local_read_bar(&mut self, local: Local, span: Span, ty: Type) -> Port {
    let wire = self.new_wire(span, ty);
    self.local_read_bar_to(local, wire.neg);
    wire.pos
  }

  pub fn local_bar_write(&mut self, local: Local, span: Span, ty: Type) -> Port {
    let wire = self.new_wire(span, ty);
    self.local_bar_write_to(local, wire.pos);
    wire.neg
  }

  pub fn local_read_write(&mut self, local: Local, span: Span, ty: Type) -> (Port, Port) {
    let o = self.new_wire(span, ty);
    let i = self.new_wire(span, ty);
    self.local_read_write_to(local, o.neg, i.pos);
    (o.pos, i.neg)
  }

  pub fn local_bar(&mut self, local: Local) {
    self.steps.push(Step::Invoke(local, Invocation::Bar));
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

#[derive(Debug, Clone)]
pub struct VirLocal {
  pub span: Span,
  pub ty: Type,
  pub inv: Inverted,
  pub fork: Option<FnRelId>,
  pub drop: Option<FnRelId>,
}

impl VirLocal {
  pub fn new<'core>(
    core: &'core Core<'core>,
    chart: &Chart<'core>,
    sigs: &Signatures<'core>,
    def: DefId,
    generics: GenericsId,
    types: &mut Types<'core>,
    rels: &mut Rels,
    span: Span,
    ty: Type,
  ) -> VirLocal {
    let flex =
      Finder::new(core, chart, sigs, def, generics, span).find_flex(types, ty).unwrap_or_default();
    VirLocal {
      span,
      ty,
      inv: flex.inv,
      fork: flex.fork.map(|impl_| rels.fork_rel(chart, impl_)),
      drop: flex.drop.map(|impl_| rels.drop_rel(chart, impl_)),
    }
  }
}
