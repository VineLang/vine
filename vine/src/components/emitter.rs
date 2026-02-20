use std::{collections::BTreeMap, mem::take};

use hedera::{
  guide,
  net::{FlatNet, FlatNode, Wire},
};
use vine_util::idx::{Counter, IdxVec};

use crate::{
  compiler::Guide,
  features::{debug::main_net_debug, local::LocalEmissionState},
  structures::{
    chart::Chart,
    resolutions::{ConstRelId, FnRelId, Fragment},
    signatures::Signatures,
    specializations::Specializations,
    template::{Template, TemplateNode},
    tir::Local,
    vir::{Header, Interface, InterfaceKind, Port, PortKind, Stage, StageId, Step, Transfer, Vir},
  },
};

pub fn emit(
  debug: bool,
  chart: &Chart,
  sigs: &Signatures,
  fragment: &Fragment,
  vir: &Vir,
  specs: &mut Specializations,
) -> Template {
  let mut emitter = Emitter {
    chart,
    sigs,
    fragment,
    vir,
    specs,
    locals: BTreeMap::new(),
    net: FlatNet::default(),
    wire_offset: 0,
    wires: Counter::default(),
    debug_state: None,
    debug,
  };

  Template { stages: IdxVec::from_iter(vir.stages.values().map(|stage| emitter.emit_stage(stage))) }
}

pub(crate) struct Emitter<'a> {
  pub(crate) chart: &'a Chart,
  pub(crate) sigs: &'a Signatures,
  pub(crate) fragment: &'a Fragment,
  pub(crate) vir: &'a Vir,
  pub(crate) specs: &'a mut Specializations,
  pub(crate) locals: BTreeMap<Local, LocalEmissionState>,
  pub(crate) net: FlatNet<TemplateNode>,
  pub(crate) wire_offset: usize,
  pub(crate) wires: Counter<usize>,
  pub(crate) debug: bool,
  pub(crate) debug_state: Option<(Wire, Wire)>,
  pub(crate) guide: Guide,
}

impl<'a> Emitter<'a> {
  pub fn emit_stage(&mut self, stage: &Stage) -> Option<FlatNet<TemplateNode>> {
    let interface = &self.vir.interfaces[stage.interface];
    (interface.incoming != 0 && !interface.inline()).then(|| {
      self.wire_offset = 0;
      self.wires.0 = stage.wires.0.0;
      let root = self.emit_interface(interface, false);
      let root = self.emit_header(&stage.header, root);
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
      && !matches!(interface.kind, InterfaceKind::Fn { .. } | InterfaceKind::Inspect(..))
    {
      target = Tree::Comb("dbg".into(), Box::new(self.tap_debug()), Box::new(target));
    }

    let pair = match &interface.kind {
      InterfaceKind::Unconditional(stage) => (self.emit_stage_rel(*stage), target),
      InterfaceKind::Branch(zero, non_zero) => (
        self.emit_port(transfer.data.as_ref().unwrap()),
        Tree::Branch(
          Box::new(self.emit_stage_rel(*zero)),
          Box::new(self.emit_stage_rel(*non_zero)),
          Box::new(target),
        ),
      ),
      InterfaceKind::Match(_, stages) => (
        self.emit_port(transfer.data.as_ref().unwrap()),
        Tree::n_ary("enum", stages.iter().map(|&s| self.emit_stage_rel(s)).chain([target])),
      ),
      InterfaceKind::Fn { .. } | InterfaceKind::Inspect(..) => {
        (self.emit_port(transfer.data.as_ref().unwrap()), target)
      }
    };
    self.pairs.push(pair);
  }

  fn inline_stage(&mut self, stage: &Stage) {
    let prev_wire_offset = self.wire_offset;
    self.wire_offset = self.wires.peek_next();
    self.wires.0 += stage.wires.0.0;
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
      Tree::n_ary(
        "x",
        locals
          .iter()
          .flat_map(|&local| {
            let (read, _, write) = interface.wires.get(&local).copied().unwrap_or_default();
            let (read, write) = self.emit_compound_invocation(local, read, true, write);
            [read, write].into_iter().flatten()
          })
          .chain([Tree::Erase]),
      )
    } else {
      Tree::n_ary(
        "x",
        interface.wires.iter().filter_map(|(&local, &(input, barrier, output))| {
          let (read, barrier, write) =
            if interior { (input, barrier, output) } else { (output, true, input) };
          let (read, write) = self.emit_compound_invocation(local, read, barrier, write);
          let wires = if interior { (read, write) } else { (write, read) };
          match wires {
            (None, None) => None,
            (None, Some(x)) | (Some(x), None) => Some(x),
            (Some(x), Some(y)) => Some(Tree::Comb("x".into(), Box::new(x), Box::new(y))),
          }
        }),
      )
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
      Step::Call(span, rel, recv, args, ret) => self.emit_call(*span, *rel, recv, args, ret),
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
      Step::InlineIvy(binds, out, net) => {
        self.emit_inline_ivy(binds, out, net);
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
      PortKind::Error(_) => self.net.insert(self.guide.eraser, []),
      PortKind::Nil => self.net.insert(self.guide.tuple, []),
      PortKind::N32(n) => self.net.insert(self.guide.n32.with_data(n), []),
      PortKind::F32(n) => self.net.insert(self.guide.f32.with_data(n), []),
      PortKind::F64(n) => self.net.insert(self.guide.f64.with_data(n), []),
      PortKind::Nat(ref n) => self.emit_nat(n),
      PortKind::Wire(_, w) => Wire(self.wire_offset + w.0),
    }
  }

  fn emit_header(&mut self, header: &Header, root: Wire) -> Wire {
    match header {
      Header::None => self.with_debug(root),
      Header::Match(enum_id, variant_id, data) => {
        let data = self.emit_port(data);
        let data = if self.sigs.enums[*enum_id].inner.variant_is_nil[*variant_id] {
          self.pairs.push((data, Tree::Erase));
          None
        } else {
          Some(data)
        };
        Tree::n_ary("enum", data.into_iter().chain([self.with_debug(root)]))
      }
      Header::Fn(params, result) => Tree::n_ary(
        "fn",
        [self.with_debug(root)]
          .into_iter()
          .chain(params.iter().chain([result]).map(|port| self.emit_port(port))),
      ),
      Header::Fork(former, latter) => Tree::n_ary(
        "fn",
        [
          self.with_debug(Tree::Erase),
          Tree::Comb("ref".into(), Box::new(root), Box::new(self.emit_port(latter))),
          self.emit_port(former),
        ],
      ),
      Header::Drop => Tree::n_ary("fn", [self.with_debug(Tree::Erase), root, Tree::Erase]),
      Header::Entry(ports) => {
        assert_eq!(root, Tree::Erase);
        let inner = Tree::n_ary("x", ports.iter().map(|port| self.emit_port(port)));
        self.with_debug(inner)
      }
    }
  }
}

pub(crate) fn main_net(debug: bool, main: Wire) -> Net {
  if debug {
    main_net_debug(main)
  } else {
    Net {
      root: Tree::n_ary("x", [Tree::Var("io0".into()), Tree::Var("io1".into())]),
      pairs: vec![(
        main,
        Tree::n_ary(
          "fn",
          [
            Tree::Erase,
            Tree::n_ary("ref", [Tree::Var("io0".into()), Tree::Var("io1".into())]),
            Tree::Erase,
          ],
        ),
      )],
    }
  }
}
