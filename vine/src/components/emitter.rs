use std::{collections::BTreeMap, mem::take};

use ivy::ast::{Net, Tree};
use vine_util::idx::{Counter, IdxVec};

use crate::structures::{
  chart::Chart,
  core::Core,
  diag::Diag,
  resolutions::{ConstRelId, FnRelId},
  template::{Template, TemplateStage, TemplateStageRels},
  tir::Local,
  types::Inverted,
  vir::{
    Header, Interface, InterfaceKind, Invocation, Port, PortKind, Stage, StageId, Step, Transfer,
    Vir, VirLocal,
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

  let stages = IdxVec::from(Vec::from_iter(vir.stages.values().map(|stage| {
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
  })));

  Template { stages }
}

struct Emitter<'core, 'a> {
  core: &'core Core<'core>,
  chart: &'a Chart<'core>,
  vir: &'a Vir<'core>,
  locals: BTreeMap<Local, LocalState>,
  pairs: Vec<(Tree, Tree)>,
  wire_offset: usize,
  wires: Counter<usize>,
  rels: TemplateStageRels,
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

  fn finish_local(&mut self, local: &VirLocal, mut state: LocalState) {
    if state.past.is_empty() {
      state.past.push((Vec::new(), Vec::new()));
    }
    let first = &mut state.past[0];
    first.0.append(&mut state.spaces);
    first.1.append(&mut state.values);
    for (spaces, values) in state.past.into_iter() {
      let (mut sources, mut sinks) = if local.inv.0 { (spaces, values) } else { (values, spaces) };
      if local.inv.0 {
        sinks.reverse();
      }
      assert!(sources.len() <= 1);
      let source = sources.pop();
      if sinks.is_empty() {
        if let Some(source) = source {
          match local.drop {
            Some(drop_rel) => {
              let drop_tree = self.emit_fn_rel(drop_rel);
              self.pairs.push((drop_tree, Tree::n_ary("fn", [Tree::Erase, source, Tree::Erase])));
            }
            None => {
              self.core.report(Diag::CannotDrop {
                span: local.span,
                ty: self.vir.types.show(self.chart, local.ty),
              });
              self.pairs.push((source, Tree::Erase));
            }
          }
        }
      } else {
        let source = source.unwrap_or_else(|| {
          self.core.report(Diag::UninitializedVariable {
            span: local.span,
            ty: self.vir.types.show(self.chart, local.ty),
          });
          Tree::Erase
        });
        if sinks.len() == 1 {
          self.pairs.push((source, sinks.pop().unwrap()));
        } else {
          match local.fork {
            Some(fork_rel) => {
              let sink = sinks
                .into_iter()
                .reduce(|former, latter| {
                  let wire = self.new_wire();
                  let fork_tree = self.emit_fn_rel(fork_rel);
                  self.pairs.push((
                    fork_tree,
                    Tree::n_ary("fn", [Tree::Erase, Tree::n_ary("ref", [wire.0, latter]), former]),
                  ));
                  wire.1
                })
                .unwrap();
              self.pairs.push((source, sink));
            }
            None => {
              self.core.report(Diag::CannotFork {
                span: local.span,
                ty: self.vir.types.show(self.chart, local.ty),
              });
            }
          }
        }
      }
    }
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
            let read = read.then(|| {
              let w = self.new_wire();
              self.local(local).read(w.0);
              w.1
            });
            self.local(local).bar();
            let write = write.then(|| {
              let w = self.new_wire();
              self.local(local).write(w.0);
              w.1
            });
            [read, write].into_iter().flatten()
          })
          .chain([Tree::Erase]),
      )
    } else {
      Tree::n_ary(
        "x",
        interface.wires.iter().filter_map(|(&local, &(input, bar, output))| {
          let (read, bar, write) =
            if interior { (input, bar, output) } else { (output, true, input) };
          let read = read.then(|| {
            let w = self.new_wire();
            self.local(local).read(w.0);
            w.1
          });
          if bar {
            self.local(local).bar();
          }
          let write = write.then(|| {
            let w = self.new_wire();
            self.local(local).write(w.0);
            w.1
          });
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

  fn local(&mut self, local: Local) -> &mut LocalState {
    self.locals.entry(local).or_insert_with(|| LocalState {
      inv: self.vir.locals[local].inv,
      past: Vec::new(),
      spaces: Vec::new(),
      values: Vec::new(),
    })
  }

  fn emit_step(&mut self, step: &Step) {
    match step {
      Step::Invoke(local, invocation) => match invocation {
        Invocation::Bar => self.local(*local).bar(),
        Invocation::Read(port) => {
          let tree = self.emit_port(port);
          self.local(*local).read(tree)
        }
        Invocation::Write(port) => {
          let tree = self.emit_port(port);
          self.local(*local).write(tree)
        }
        Invocation::ReadBar(port) => {
          let tree = self.emit_port(port);
          self.local(*local).read_bar(tree)
        }
        Invocation::BarWrite(port) => {
          let tree = self.emit_port(port);
          self.local(*local).bar_write(tree)
        }
        Invocation::ReadWrite(a, b) => {
          let a = self.emit_port(a);
          let b = self.emit_port(b);
          self.local(*local).read_write(a, b)
        }
      },
      Step::Transfer(transfer) => self.emit_transfer(transfer),
      Step::Diverge(..) => unreachable!(),

      Step::Link(a, b) | Step::Struct(_, a, b) => {
        let a = self.emit_port(a);
        let b = self.emit_port(b);
        self.pairs.push((a, b))
      }
      Step::Call(rel, recv, args, ret) => {
        let func = self.emit_fn_rel(*rel);
        let recv = recv.as_ref().map(|p| self.emit_port(p)).unwrap_or(Tree::Erase);
        let pair = (
          func,
          Tree::n_ary(
            "fn",
            [recv].into_iter().chain(args.iter().chain([ret]).map(|p| self.emit_port(p))),
          ),
        );
        self.pairs.push(pair)
      }
      Step::Composite(port, tuple) => {
        let pair =
          (self.emit_port(port), Tree::n_ary("tup", tuple.iter().map(|p| self.emit_port(p))));
        self.pairs.push(pair)
      }
      Step::Enum(enum_id, variant_id, port, fields) => {
        let enum_def = &self.chart.enums[*enum_id];
        let wire = self.new_wire();
        let mut fields =
          Tree::n_ary("enum", fields.iter().map(|p| self.emit_port(p)).chain([wire.0]));
        let enum_ = Tree::n_ary(
          "enum",
          (0..enum_def.variants.len())
            .map(|i| if variant_id.0 == i { take(&mut fields) } else { Tree::Erase })
            .chain([wire.1]),
        );
        let pair = (self.emit_port(port), enum_);
        self.pairs.push(pair);
      }
      Step::Ref(reference, value, space) => {
        let pair = (
          self.emit_port(reference),
          Tree::Comb(
            "ref".into(),
            Box::new(self.emit_port(value)),
            Box::new(self.emit_port(space)),
          ),
        );
        self.pairs.push(pair)
      }
      Step::ExtFn(ext_fn, swap, lhs, rhs, out) => {
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
      Step::List(port, list) => {
        let end = self.new_wire();
        let buf = Tree::n_ary("tup", list.iter().map(|p| self.emit_port(p)).chain([end.0]));
        let list = Tree::n_ary("tup", [Tree::N32(list.len() as u32), buf, end.1]);
        let pair = (self.emit_port(port), list);
        self.pairs.push(pair)
      }
      Step::String(port, init, rest) => {
        let const_len =
          init.chars().count() + rest.iter().map(|x| x.1.chars().count()).sum::<usize>();
        let len = self.new_wire();
        let start = self.new_wire();
        let end = self.new_wire();
        let port = self.emit_port(port);
        self.pairs.push((
          port,
          Tree::n_ary(
            "tup",
            [
              len.0,
              Tree::n_ary("tup", init.chars().map(|c| Tree::N32(c as u32)).chain([start.0])),
              end.0,
            ],
          ),
        ));
        let mut cur_len = Tree::N32(const_len as u32);
        let mut cur_buf = start.1;
        for (port, seg) in rest {
          let next_len = self.new_wire();
          let next_buf = self.new_wire();
          let port = self.emit_port(port);
          self.pairs.push((
            port,
            Tree::n_ary(
              "tup",
              [
                Tree::ExtFn("n32_add".into(), false, Box::new(cur_len), Box::new(next_len.0)),
                cur_buf,
                Tree::n_ary("tup", seg.chars().map(|c| Tree::N32(c as u32)).chain([next_buf.0])),
              ],
            ),
          ));
          cur_len = next_len.1;
          cur_buf = next_buf.1;
        }
        self.pairs.push((cur_len, len.1));
        self.pairs.push((cur_buf, end.1));
      }
      Step::InlineIvy(binds, out, net) => {
        for (var, port) in binds {
          let port = self.emit_port(port);
          self.pairs.push((Tree::Var(var.clone()), port));
        }
        let out = self.emit_port(out);
        self.pairs.push((out, net.root.clone()));
        self.pairs.extend_from_slice(&net.pairs);
      }
    }
  }

  fn emit_fn_rel(&mut self, rel: FnRelId) -> Tree {
    let wire = self.new_wire();
    self.rels.fns.push((rel, wire.0));
    wire.1
  }

  fn emit_const_rel(&mut self, rel: ConstRelId) -> Tree {
    let wire = self.new_wire();
    self.rels.consts.push((rel, wire.0));
    wire.1
  }

  fn emit_stage_rel(&mut self, stage: StageId) -> Tree {
    let wire = self.new_wire();
    self.rels.stages.push((stage, wire.0));
    wire.1
  }

  fn emit_port(&mut self, port: &Port) -> Tree {
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

  fn new_wire(&mut self) -> (Tree, Tree) {
    let label = format!("w{}", self.wires.next());
    (Tree::Var(label.clone()), Tree::Var(label))
  }
}

#[derive(Debug)]
struct LocalState {
  inv: Inverted,
  past: Vec<(Vec<Tree>, Vec<Tree>)>,
  spaces: Vec<Tree>,
  values: Vec<Tree>,
}

impl LocalState {
  fn read(&mut self, tree: Tree) {
    self.spaces.push(tree);
    if self.inv.0 {
      self.bar();
    }
  }

  fn read_bar(&mut self, tree: Tree) {
    self.read(tree);
    self.bar();
  }

  fn write(&mut self, tree: Tree) {
    if !self.inv.0 {
      self.bar();
    }
    self.values.push(tree);
  }

  fn bar_write(&mut self, tree: Tree) {
    self.bar();
    self.write(tree);
  }

  fn read_write(&mut self, a: Tree, b: Tree) {
    self.read(a);
    self.write(b);
  }

  fn bar(&mut self) {
    if self.past.is_empty() || !self.spaces.is_empty() || !self.values.is_empty() {
      self.past.push((take(&mut self.spaces), take(&mut self.values)));
    }
  }
}
