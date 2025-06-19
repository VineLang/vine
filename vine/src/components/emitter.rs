use std::{collections::BTreeMap, fmt::Write, mem::take};

use ivy::ast::{Net, Nets, Tree};
use vine_util::idx::{Counter, IdxVec};

use crate::structures::{
  chart::{Chart, EnumDef, VariantId},
  core::Core,
  diag::Diag,
  resolutions::{FnRelId, Fragment, FragmentId},
  specializations::{Spec, SpecId, Specializations},
  tir::Local,
  types::Inverted,
  vir::{
    Header, Interface, InterfaceKind, Invocation, Port, PortKind, Stage, StageId, Step, Transfer,
    Vir, VirLocal,
  },
};

pub struct Emitter<'core, 'a> {
  pub nets: Nets,
  core: &'core Core<'core>,
  chart: &'a Chart<'core>,
  specs: &'a Specializations,
  fragments: &'a IdxVec<FragmentId, Fragment<'core>>,
  vir: &'a IdxVec<FragmentId, Vir<'core>>,
}

impl<'core, 'a> Emitter<'core, 'a> {
  pub fn new(
    core: &'core Core<'core>,
    chart: &'a Chart<'core>,
    specs: &'a Specializations,
    fragments: &'a IdxVec<FragmentId, Fragment<'core>>,
    vir: &'a IdxVec<FragmentId, Vir<'core>>,
  ) -> Self {
    Emitter { nets: Nets::default(), core, chart, specs, fragments, vir }
  }

  pub fn emit_main(&mut self, main: FragmentId) {
    let path = self.fragments[main].path;
    let vir = &self.vir[main];
    let func = *vir.closures.last().unwrap();
    let InterfaceKind::Fn { call, .. } = vir.interfaces[func].kind else { unreachable!() };
    let global = format!("{path}::{}", call.0);
    self.nets.insert("::".into(), Net { root: Tree::Global(global), pairs: Vec::new() });
  }

  pub fn emit_spec(&mut self, spec_id: SpecId) {
    let spec = self.specs.specs[spec_id].as_ref().unwrap();
    let vir = &self.vir[spec.fragment];

    let mut emitter = VirEmitter {
      core: self.core,
      chart: self.chart,
      specs: self.specs,
      fragments: self.fragments,
      spec_id,
      spec,
      vir,
      locals: BTreeMap::new(),
      pairs: Vec::new(),
      wire_offset: 0,
      wires: Counter::default(),
    };

    for stage in vir.stages.values() {
      let interface = &vir.interfaces[stage.interface];
      if interface.incoming != 0 && !interface.inline() {
        emitter.wire_offset = 0;
        emitter.wires.0 = stage.wires.0 .0;
        let root = emitter.emit_interface(interface, false);
        let root = emitter.emit_header(&stage.header, root);
        emitter._emit_stage(stage);
        for (local, state) in take(&mut emitter.locals) {
          emitter.finish_local(&vir.locals[local], state);
        }
        let net = Net { root, pairs: take(&mut emitter.pairs) };
        self.nets.insert(emitter.stage_name(spec_id, stage.id), net);
      }
    }
  }
}

struct VirEmitter<'core, 'a> {
  core: &'core Core<'core>,
  chart: &'a Chart<'core>,
  specs: &'a Specializations,
  fragments: &'a IdxVec<FragmentId, Fragment<'core>>,
  spec_id: SpecId,
  spec: &'a Spec,
  vir: &'a Vir<'core>,
  locals: BTreeMap<Local, LocalState>,
  pairs: Vec<(Tree, Tree)>,
  wire_offset: usize,
  wires: Counter<usize>,
}

impl<'core, 'a> VirEmitter<'core, 'a> {
  pub fn emit_transfer(&mut self, transfer: &Transfer) {
    let interface = &self.vir.interfaces[transfer.interface];
    if interface.inline() {
      let InterfaceKind::Unconditional(stage) = interface.kind else { unreachable!() };
      return self.inline_stage(&self.vir.stages[stage]);
    }

    let target = self.emit_interface(interface, true);

    self.pairs.push(match &interface.kind {
      InterfaceKind::Unconditional(stage) => (self.emit_stage_node(*stage), target),
      InterfaceKind::Branch(zero, non_zero) => (
        self.emit_port(transfer.data.as_ref().unwrap()),
        Tree::Branch(
          Box::new(self.emit_stage_node(*zero)),
          Box::new(self.emit_stage_node(*non_zero)),
          Box::new(target),
        ),
      ),
      InterfaceKind::Match(_, stages) => (
        self.emit_port(transfer.data.as_ref().unwrap()),
        Tree::n_ary("enum", stages.iter().map(|&s| self.emit_stage_node(s)).chain([target])),
      ),
      InterfaceKind::Fn { .. } => (self.emit_port(transfer.data.as_ref().unwrap()), target),
    });
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
              let drop_tree = self.emit_fn_rel(&drop_rel);
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
              let fork_tree = self.emit_fn_rel(&fork_rel);
              let sink = sinks
                .into_iter()
                .reduce(|former, latter| {
                  let wire = self.new_wire();
                  self.pairs.push((
                    fork_tree.clone(),
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

  fn emit_stage_node(&self, stage_id: StageId) -> Tree {
    Tree::Global(self.stage_name(self.spec_id, stage_id))
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
    let wire_offset = self.wire_offset;
    let emit_port = |p| Self::_emit_port(wire_offset, self.specs, self.fragments, self.spec, p);
    match step {
      Step::Invoke(local, invocation) => match invocation {
        Invocation::Bar => self.local(*local).bar(),
        Invocation::Read(port) => self.local(*local).read(emit_port(port)),
        Invocation::Write(port) => self.local(*local).write(emit_port(port)),
        Invocation::ReadBar(port) => self.local(*local).read_bar(emit_port(port)),
        Invocation::BarWrite(port) => self.local(*local).bar_write(emit_port(port)),
        Invocation::ReadWrite(a, b) => self.local(*local).read_write(emit_port(a), emit_port(b)),
      },
      Step::Transfer(transfer) => self.emit_transfer(transfer),
      Step::Diverge(..) => unreachable!(),

      Step::Link(a, b) | Step::Struct(_, a, b) => self.pairs.push((emit_port(a), emit_port(b))),
      Step::Call(rel, recv, args, ret) => {
        let func = self.emit_fn_rel(rel);
        self.pairs.push((
          func,
          Tree::n_ary(
            "fn",
            [recv.as_ref().map(emit_port).unwrap_or(Tree::Erase)]
              .into_iter()
              .chain(args.iter().chain([ret]).map(emit_port)),
          ),
        ))
      }
      Step::Composite(port, tuple) => {
        self.pairs.push((emit_port(port), Tree::n_ary("tup", tuple.iter().map(emit_port))))
      }
      Step::Enum(enum_id, variant_id, port, fields) => {
        let enum_def = &self.chart.enums[*enum_id];
        let fields = fields.iter().map(emit_port);
        let enum_ = make_enum(*variant_id, enum_def, || self.new_wire(), fields);
        self.pairs.push((emit_port(port), enum_));
      }
      Step::Ref(reference, value, space) => self.pairs.push((
        emit_port(reference),
        Tree::Comb("ref".into(), Box::new(emit_port(value)), Box::new(emit_port(space))),
      )),
      Step::ExtFn(ext_fn, swap, lhs, rhs, out) => self.pairs.push((
        emit_port(lhs),
        Tree::ExtFn(ext_fn.to_string(), *swap, Box::new(emit_port(rhs)), Box::new(emit_port(out))),
      )),
      Step::List(port, list) => {
        let str = self.emit_list(list.iter().map(emit_port));
        self.pairs.push((emit_port(port), str))
      }
      Step::String(port, init, rest) => {
        let const_len =
          init.chars().count() + rest.iter().map(|x| x.1.chars().count()).sum::<usize>();
        let len = self.new_wire();
        let start = self.new_wire();
        let end = self.new_wire();
        self.pairs.push((
          emit_port(port),
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
          self.pairs.push((
            emit_port(port),
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
          self.pairs.push((Tree::Var(var.clone()), emit_port(port)));
        }
        self.pairs.push((emit_port(out), net.root.clone()));
        self.pairs.extend_from_slice(&net.pairs);
      }
    }
  }

  fn emit_fn_rel(&mut self, rel: &FnRelId) -> Tree {
    match self.spec.rels.fns[*rel] {
      Ok((spec_id, stage_id)) => Tree::Global(self.stage_name(spec_id, stage_id)),
      Err(_) => Tree::Erase,
    }
  }

  fn emit_port(&self, port: &Port) -> Tree {
    Self::_emit_port(self.wire_offset, self.specs, self.fragments, self.spec, port)
  }

  fn _emit_port(
    wire_offset: usize,
    specs: &Specializations,
    fragments: &IdxVec<FragmentId, Fragment<'core>>,
    spec: &Spec,
    port: &Port,
  ) -> Tree {
    match port.kind {
      PortKind::Error(_) => Tree::Erase,
      PortKind::Nil => Tree::Erase,
      PortKind::N32(n) => Tree::N32(n),
      PortKind::F32(f) => Tree::F32(f),
      PortKind::Wire(_, w) => Tree::Var(format!("w{}", wire_offset + w.0)),
      PortKind::ConstRel(rel) => match spec.rels.consts[rel] {
        Ok(spec_id) => Tree::Global(Self::_stage_name(specs, fragments, spec_id, StageId(0))),
        Err(_) => Tree::Erase,
      },
    }
  }

  fn emit_list(
    &mut self,
    ports: impl IntoIterator<IntoIter: DoubleEndedIterator<Item = Tree>>,
  ) -> Tree {
    let end = self.new_wire();
    let mut len = 0;
    let buf = Tree::n_ary("tup", ports.into_iter().inspect(|_| len += 1).chain([end.0]));
    Tree::n_ary("tup", [Tree::N32(len), buf, end.1])
  }

  fn emit_header(&self, header: &Header, root: Tree) -> Tree {
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
    }
  }

  fn new_wire(&mut self) -> (Tree, Tree) {
    let label = format!("w{}", self.wires.next());
    (Tree::Var(label.clone()), Tree::Var(label))
  }

  pub fn stage_name(&self, spec_id: SpecId, stage_id: StageId) -> String {
    Self::_stage_name(self.specs, self.fragments, spec_id, stage_id)
  }

  fn _stage_name(
    specs: &Specializations,
    fragments: &IdxVec<FragmentId, Fragment<'core>>,
    spec_id: SpecId,
    stage_id: StageId,
  ) -> String {
    let spec = specs.specs[spec_id].as_ref().unwrap();
    let path = fragments[spec.fragment].path;
    let mut name = path.to_owned();
    if !spec.singular {
      write!(name, "::{}", spec.index).unwrap();
    }
    if stage_id.0 != 0 {
      write!(name, "::{}", stage_id.0).unwrap();
    }
    name
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

fn make_enum(
  variant_id: VariantId,
  enum_def: &EnumDef,
  mut new_wire: impl FnMut() -> (Tree, Tree),
  fields: impl DoubleEndedIterator<Item = Tree>,
) -> Tree {
  let wire = new_wire();
  let mut fields = Tree::n_ary("enum", fields.chain([wire.0]));
  Tree::n_ary(
    "enum",
    (0..enum_def.variants.len())
      .map(|i| if variant_id.0 == i { take(&mut fields) } else { Tree::Erase })
      .chain([wire.1]),
  )
}
