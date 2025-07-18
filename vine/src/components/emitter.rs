use std::{collections::BTreeMap, mem::take};

use ivy::ast::{Net, Tree};
use vine_util::idx::{Counter, IdxVec};

use crate::{
  features::local::LocalEmissionState,
  structures::{
    chart::Chart,
    core::Core,
    resolutions::{ConstRelId, FnRelId},
    template::{Template, TemplateStage, TemplateStageRels},
    tir::Local,
    vir::{Header, Interface, InterfaceKind, Port, PortKind, Stage, StageId, Step, Transfer, Vir},
  },
};

pub fn emit<'core>(core: &'core Core<'core>, chart: &Chart<'core>, vir: &Vir<'core>) -> Template {
  let mut emitter = Emitter {
    core,
    chart,
    vir,
    locals: BTreeMap::new(),
    pairs: Vec::new(),
    wire_offset: 0,
    wires: Counter::default(),
    rels: TemplateStageRels::default(),
  };

  let stages = IdxVec::from_iter(vir.stages.values().map(|stage| {
    let interface = &vir.interfaces[stage.interface];
    (interface.incoming != 0 && !interface.inline()).then(|| {
      emitter.wire_offset = 0;
      emitter.wires.0 = stage.wires.0 .0;
      let root = emitter.emit_interface(interface, false);
      let root = emitter.emit_header(&stage.header, root);
      emitter._emit_stage(stage);
      for (local, state) in take(&mut emitter.locals) {
        emitter.finish_local(&vir.locals[local], state);
      }
      let net = Net { root, pairs: take(&mut emitter.pairs) };
      TemplateStage { net, rels: take(&mut emitter.rels) }
    })
  }));

  Template { stages }
}

pub(crate) struct Emitter<'core, 'a> {
  pub(crate) core: &'core Core<'core>,
  pub(crate) chart: &'a Chart<'core>,
  pub(crate) vir: &'a Vir<'core>,
  pub(crate) locals: BTreeMap<Local, LocalEmissionState>,
  pub(crate) pairs: Vec<(Tree, Tree)>,
  pub(crate) wire_offset: usize,
  pub(crate) wires: Counter<usize>,
  pub(crate) rels: TemplateStageRels,
}

impl<'core, 'a> Emitter<'core, 'a> {
  pub fn emit_transfer(&mut self, transfer: &Transfer) {
    let interface = &self.vir.interfaces[transfer.interface];
    if interface.inline() {
      let InterfaceKind::Unconditional(stage) = interface.kind else { unreachable!() };
      return self.inline_stage(&self.vir.stages[stage]);
    }

    let target = self.emit_interface(interface, true);

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
    self.wires.0 += stage.wires.0 .0;
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

  fn emit_interface(&mut self, interface: &Interface, interior: bool) -> Tree {
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
      Step::Invoke(local, invocation) => self.emit_invocation(local, invocation),
      Step::Transfer(transfer) => self.emit_transfer(transfer),
      Step::Diverge(..) => unreachable!(),

      Step::Link(a, b) => {
        self.emit_link(a, b);
      }
      Step::Struct(id, a, b) => self.emit_struct(*id, a, b),
      Step::Call(rel, recv, args, ret) => self.emit_call(*rel, recv, args, ret),
      Step::Composite(port, tuple) => self.emit_composite(port, tuple),
      Step::Ref(reference, value, space) => self.emit_ref(reference, value, space),
      Step::Enum(enum_id, variant_id, port, fields) => {
        self.emit_enum(*enum_id, *variant_id, port, fields)
      }
      Step::ExtFn(ext_fn, swap, lhs, rhs, out) => {
        self.emit_ext_fn(ext_fn, swap, lhs, rhs, out);
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
    self.pairs.push((a, b))
  }

  fn emit_ext_fn(
    &mut self,
    ext_fn: &&'static str,
    swap: &bool,
    lhs: &Port,
    rhs: &Port,
    out: &Port,
  ) {
    let pair = (
      self.emit_port(lhs),
      Tree::ExtFn(
        ext_fn.to_string(),
        *swap,
        Box::new(self.emit_port(rhs)),
        Box::new(self.emit_port(out)),
      ),
    );
    self.pairs.push(pair)
  }

  pub(crate) fn emit_fn_rel(&mut self, rel: FnRelId) -> Tree {
    let wire = self.new_wire();
    self.rels.fns.push((rel, wire.0));
    wire.1
  }

  pub(crate) fn emit_const_rel(&mut self, rel: ConstRelId) -> Tree {
    let wire = self.new_wire();
    self.rels.consts.push((rel, wire.0));
    wire.1
  }

  pub(crate) fn emit_stage_rel(&mut self, stage: StageId) -> Tree {
    let wire = self.new_wire();
    self.rels.stages.push((stage, wire.0));
    wire.1
  }

  pub(crate) fn emit_port(&mut self, port: &Port) -> Tree {
    match port.kind {
      PortKind::Error(_) => Tree::Erase,
      PortKind::Nil => Tree::Erase,
      PortKind::N32(n) => Tree::N32(n),
      PortKind::F32(f) => Tree::F32(f),
      PortKind::Wire(_, w) => Tree::Var(format!("w{}", self.wire_offset + w.0)),
      PortKind::ConstRel(rel) => self.emit_const_rel(rel),
    }
  }

  fn emit_header(&mut self, header: &Header, root: Tree) -> Tree {
    match header {
      Header::None => root,
      Header::Match(None) => root,
      Header::Match(Some(data)) => {
        Tree::Comb("enum".into(), Box::new(self.emit_port(data)), Box::new(root))
      }
      Header::Fn(params, result) => Tree::n_ary(
        "fn",
        [root].into_iter().chain(params.iter().chain([result]).map(|port| self.emit_port(port))),
      ),
      Header::Fork(former, latter) => Tree::n_ary(
        "fn",
        [
          Tree::Erase,
          Tree::Comb("ref".into(), Box::new(root), Box::new(self.emit_port(latter))),
          self.emit_port(former),
        ],
      ),
      Header::Drop => Tree::n_ary("fn", [Tree::Erase, root, Tree::Erase]),
      Header::Entry(ports) => {
        assert_eq!(root, Tree::Erase);
        Tree::n_ary("x", ports.iter().map(|port| self.emit_port(port)))
      }
    }
  }

  pub(crate) fn new_wire(&mut self) -> (Tree, Tree) {
    let label = format!("w{}", self.wires.next());
    (Tree::Var(label.clone()), Tree::Var(label))
  }
}
