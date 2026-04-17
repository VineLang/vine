use std::{collections::BTreeMap, mem::take};

use ivy::{
  name::{NameId, Table},
  net::{FlatNet, Wire},
};
use vine_util::idx::IdxVec;

use crate::{
  compiler::Guide,
  features::{debug::main_net_debug, enum_::enum_name, local::LocalEmissionState},
  structures::{
    chart::Chart,
    resolutions::{ConstRelId, FnRelId, Fragment},
    specializations::Specializations,
    template::{Template, TemplateNode},
    tir::Local,
    vir::{Header, Interface, InterfaceKind, Port, PortKind, Stage, StageId, Step, Transfer, Vir},
  },
};

pub fn emit(
  debug: bool,
  chart: &Chart,
  fragment: &Fragment,
  vir: &Vir,
  specs: &mut Specializations,
  table: &mut Table,
  guide: &Guide,
) -> Template {
  let mut emitter = Emitter {
    chart,
    fragment,
    vir,
    specs,
    table,
    guide,
    locals: BTreeMap::new(),
    net: FlatNet::default(),
    wire_offset: 0,
    debug_state: None,
    debug,
  };

  Template { stages: IdxVec::from_iter(vir.stages.values().map(|stage| emitter.emit_stage(stage))) }
}

pub(crate) struct Emitter<'a> {
  pub(crate) chart: &'a Chart,
  pub(crate) fragment: &'a Fragment,
  pub(crate) vir: &'a Vir,
  pub(crate) specs: &'a mut Specializations,
  pub(crate) table: &'a mut Table,
  pub(crate) guide: &'a Guide,
  pub(crate) locals: BTreeMap<Local, LocalEmissionState>,
  pub(crate) net: FlatNet<TemplateNode>,
  pub(crate) wire_offset: usize,
  pub(crate) debug: bool,
  pub(crate) debug_state: Option<(Wire, Wire)>,
}

impl<'a> Emitter<'a> {
  pub fn emit_stage(&mut self, stage: &Stage) -> Option<FlatNet<TemplateNode>> {
    let interface = &self.vir.interfaces[stage.interface];
    (interface.incoming != 0 && !interface.inline()).then(|| {
      self.wire_offset = 0;
      self.net.wires.0.0 = stage.wires.0.0;
      let root = self.emit_header(&stage.header, interface);
      self.net.free.push(root);
      self._emit_stage(stage);
      for (local, state) in take(&mut self.locals) {
        self.finish_local(&self.vir.locals[local], state);
      }
      if self.debug {
        let (a, b) = self.debug_state.take().unwrap();
        self.net.link(a, b);
      }
      take(&mut self.net)
    })
  }

  pub fn emit_transfer(&mut self, transfer: &Transfer) {
    let interface = &self.vir.interfaces[transfer.interface];
    if interface.inline() {
      let InterfaceKind::Unconditional(stage) = interface.kind else { unreachable!() };
      return self.inline_stage(&self.vir.stages[stage]);
    }

    let mut target = self.emit_interface(interface, true);

    if self.debug
      && !matches!(interface.kind, InterfaceKind::Closure { .. } | InterfaceKind::Inspect(..))
    {
      let dbg = self.tap_debug();
      target = self.net.make(self.guide.dbg, [dbg, target]);
    }

    match &interface.kind {
      InterfaceKind::Unconditional(stage) => {
        let stage = self.emit_stage_rel(*stage);
        self.net.link(stage, target);
      }
      InterfaceKind::Branch(then, else_) => {
        let bool = self.emit_port(transfer.data.as_ref().unwrap());
        self.net.push(TemplateNode::If(*then, *else_, bool, target));
      }
      InterfaceKind::Match(enum_id, stages) => {
        let enum_name = enum_name(self.table, self.guide, self.chart, *enum_id);
        let enum_ = self.emit_port(transfer.data.as_ref().unwrap());
        self.net.push(TemplateNode::Match(enum_name, stages.clone(), enum_, target));
        self.emit_port(transfer.data.as_ref().unwrap());
      }
      InterfaceKind::Closure { .. } | InterfaceKind::Inspect(..) => {
        let consumer = self.emit_port(transfer.data.as_ref().unwrap());
        self.net.link(consumer, target);
      }
    };
  }

  fn inline_stage(&mut self, stage: &Stage) {
    let prev_wire_offset = self.wire_offset;
    self.wire_offset = self.net.wires.peek_next().0;
    self.net.wires.0.0 += stage.wires.0.0;
    self._emit_stage(stage);
    self.wire_offset = prev_wire_offset;
  }

  fn _emit_stage(&mut self, stage: &Stage) {
    for local in &stage.declarations {
      if let Some(state) = self.locals.remove(local) {
        self.finish_local(&self.vir.locals[*local], state);
      }
    }
    for step in &stage.steps {
      self.emit_step(step);
    }
  }

  fn emit_interface(&mut self, interface: &Interface, interior: bool) -> Wire {
    if let InterfaceKind::Inspect(locals) = &interface.kind {
      assert!(interior);
      let mut entries = locals
        .iter()
        .flat_map(|&local| {
          let (read, _, write) = interface.wires.get(&local).copied().unwrap_or_default();
          let (read, write) = self.emit_compound_invocation(local, read, true, write);
          [read, write].into_iter().flatten()
        })
        .collect::<Vec<_>>();
      entries.push(self.net.make(self.guide.eraser, []));
      self.net.make(self.guide.interface, entries)
    } else {
      let vars = interface
        .wires
        .iter()
        .filter_map(|(&local, &(input, barrier, output))| {
          let (read, barrier, write) =
            if interior { (input, barrier, output) } else { (output, true, input) };
          let (read, write) = self.emit_compound_invocation(local, read, barrier, write);
          let wires = if interior { (read, write) } else { (write, read) };
          match wires {
            (None, None) => None,
            (None, Some(x)) | (Some(x), None) => Some(x),
            (Some(x), Some(y)) => Some(self.net.make(self.guide.ref_, [x, y])),
          }
        })
        .collect::<Vec<_>>();
      self.net.make(self.guide.interface, vars)
    }
  }

  fn emit_step(&mut self, step: &Step) {
    match step {
      Step::Invoke(_, local, invocation) => self.emit_invocation(local, invocation),
      Step::Transfer(transfer) => self.emit_transfer(transfer),
      Step::Diverge(..) => unreachable!(),
      Step::Link(a, b) => {
        self.emit_link(a, b);
      }
      Step::Rewrap(a, b) => {
        let a = self.emit_port(a);
        let b = self.emit_port(b);
        self.net.link(a, b);
      }
      Step::Call(span, rel, args, ret) => self.emit_call(*span, *rel, args, ret),
      Step::Const(span, rel, out) => {
        self.emit_const(span, rel, out);
      }
      Step::Composite(port, tuple) => self.emit_composite(port, tuple),
      Step::Ref(reference, value, space) => self.emit_ref(reference, value, space),
      Step::Enum(enum_id, variant_id, port, fields) => {
        self.emit_enum(*enum_id, *variant_id, port, fields)
      }
      Step::Union(union_id, variant_id, port, fields) => {
        self.emit_union(*union_id, *variant_id, port, fields)
      }
      Step::List(port, list) => self.emit_list(port, list),
      Step::String(port, init, rest) => self.emit_string(port, init, rest),
      Step::InlineIvy(root, table, net, interpolations) => {
        self.emit_inline_ivy(root, table, net, interpolations);
      }
      Step::BoolNot(a, b) => {
        let a = self.emit_port(a);
        let b = self.emit_port(b);
        self.net.add(
          self.guide.ext.with_payload(1u32).with_children([self.guide.bool_not]),
          a,
          [b],
        );
      }
      Step::BoolAnd(a, b, c) => {
        let a = self.emit_port(a);
        let b = self.emit_port(b);
        let c = self.emit_port(c);
        self.net.add(
          self.guide.ext.with_payload(2u32).with_children([self.guide.bool_and]),
          a,
          [b, c],
        );
      }
    }
  }

  fn emit_link(&mut self, a: &Port, b: &Port) {
    let a = self.emit_port(a);
    let b = self.emit_port(b);
    self.net.link(a, b);
  }

  pub(crate) fn emit_fn_rel(&mut self, rel: FnRelId) -> Wire {
    let wire = self.net.wire();
    self.net.push(TemplateNode::Fn(rel, wire));
    wire
  }

  pub(crate) fn emit_const_rel(&mut self, rel: ConstRelId) -> Wire {
    let wire = self.net.wire();
    self.net.push(TemplateNode::Const(rel, wire));
    wire
  }

  pub(crate) fn emit_stage_rel(&mut self, stage: StageId) -> Wire {
    let wire = self.net.wire();
    self.net.push(TemplateNode::Stage(stage, wire));
    wire
  }

  pub(crate) fn emit_port(&mut self, port: &Port) -> Wire {
    match port.kind {
      PortKind::Error(_) => self.net.make(self.guide.eraser, []),
      PortKind::Nil => self.net.make(self.guide.tuple, []),
      PortKind::Bool(b) => self.net.make(self.guide.bool.with_payload(b), []),
      PortKind::N32(n) => self.net.make(self.guide.n32.with_payload(n), []),
      PortKind::F32(n) => self.net.make(self.guide.f32.with_payload(n), []),
      PortKind::F64(n) => self.net.make(self.guide.f64.with_payload(n), []),
      PortKind::Nat(ref n) => self.emit_nat(n),
      PortKind::Wire(_, w) => Wire(self.wire_offset + w.0),
    }
  }

  fn emit_header(&mut self, header: &Header, interface: &Interface) -> Wire {
    match header {
      Header::Const(port) => {
        let port = self.emit_port(port);
        self.with_debug(port)
      }
      Header::Fn(params, result) => {
        let wires = params.iter().chain([result]).map(|i| self.emit_port(i)).collect::<Vec<_>>();
        let func = self.net.make(self.guide.fn_, wires);
        self.with_debug(func)
      }
      Header::Bare => {
        let interface = self.emit_interface(interface, false);
        self.with_debug(interface)
      }
      Header::Match(_, _, data) => {
        let interface = self.emit_interface(interface, false);
        let data = self.emit_port(data);
        let ctx = self.with_debug(interface);
        self.net.make(self.guide.interface, [data, ctx])
      }
      Header::Closure(params, result) => {
        let interface = self.emit_interface(interface, false);
        let params = params.iter().map(|i| self.emit_port(i)).collect::<Vec<_>>();
        let result = self.emit_port(result);
        let tuple = self.net.make(self.guide.tuple, params);
        let func = self.net.make(self.guide.fn_, [interface, tuple, result]);
        self.with_debug(func)
      }
      Header::Fork(former, latter) => {
        let interface = self.emit_interface(interface, false);
        let former = self.emit_port(former);
        let latter = self.emit_port(latter);
        let ref_ = self.net.make(self.guide.ref_, [interface, latter]);
        let func = self.net.make(self.guide.fn_, [ref_, former]);
        self.with_debug(func)
      }
      Header::Drop => {
        let interface = self.emit_interface(interface, false);
        let nil = self.net.make(self.guide.tuple, []);
        let func = self.net.make(self.guide.fn_, [interface, nil]);
        self.with_debug(func)
      }
      Header::Entry(ports) => {
        let ports = ports.iter().map(|p| self.emit_port(p)).collect::<Vec<_>>();
        let interface = self.net.make(self.guide.interface, ports);
        self.with_debug(interface)
      }
    }
  }
}

pub(crate) fn main_net(debug: bool, main: NameId, guide: &Guide) -> FlatNet {
  if debug {
    main_net_debug(main, guide)
  } else {
    let mut net = FlatNet::default();
    let [io0, io1] = net.wires();
    let free = net.make(guide.tuple, [io0, io1]);
    net.free.push(free);
    let main = net.make(guide.graft.with_children([main]), []);
    let ref_ = net.make(guide.ref_, [io0, io1]);
    let nil = net.make(guide.tuple, []);
    net.add(guide.fn_, main, [ref_, nil]);
    net
  }
}
