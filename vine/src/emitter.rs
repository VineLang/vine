use std::{collections::BTreeMap, fmt::Write, mem::take};

use ivy::ast::{Net, Nets, Tree};
use vine_util::idx::{Counter, IdxVec};

use crate::{
  analyzer::usage::Usage,
  ast::Local,
  chart::{AdtDef, Chart, ValueDef, ValueDefId, ValueDefKind, VariantId},
  specializer::{Spec, SpecId, Specializations},
  vir::{
    Interface, InterfaceId, InterfaceKind, Invocation, Port, Stage, StageId, Step, Transfer, VIR,
  },
};

pub struct Emitter<'core, 'a> {
  pub nets: Nets,
  chart: &'a Chart<'core>,
  specs: &'a Specializations<'core>,
  dup_labels: Counter<usize>,
}

impl<'core, 'a> Emitter<'core, 'a> {
  pub fn new(chart: &'a Chart<'core>, specs: &'a Specializations<'core>) -> Self {
    Emitter { nets: Nets::default(), chart, specs, dup_labels: Counter::default() }
  }

  pub fn emit_spec(&mut self, spec: SpecId, vir: &IdxVec<ValueDefId, Option<VIR>>) {
    let spec = self.specs.specs[spec].as_ref().unwrap();
    let value_id = spec.value;
    let value = &self.chart.values[value_id];
    let Some(vir) = &vir[value_id] else { return };
    let path = self.chart.defs[value.def].path;
    self.emit_vir(vir, path, spec)
  }

  pub fn emit_vir(&mut self, vir: &VIR, path: &str, spec: &Spec) {
    let mut emitter = VirEmitter {
      chart: self.chart,
      path,
      stages: &vir.stages,
      interfaces: &vir.interfaces,
      specs: self.specs,
      spec,
      locals: BTreeMap::new(),
      pairs: Vec::new(),
      wire_offset: 0,
      wires: Counter::default(),
      dup_labels: self.dup_labels,
    };

    for stage in vir.stages.values() {
      let interface = &vir.interfaces[stage.interface];
      if interface.incoming != 0 && !interface.inline() {
        emitter.wire_offset = 0;
        emitter.wires.0 = stage.wires.0 .0;
        let root = emitter.emit_interface(interface, true);
        let root =
          Tree::n_ary("enum", stage.header.iter().map(|p| emitter.emit_port(p)).chain([root]));
        emitter._emit_stage(stage);
        for (_, local) in take(&mut emitter.locals) {
          emitter.finish_local(local);
        }
        let net = Net { root, pairs: take(&mut emitter.pairs) };
        self.nets.insert(emitter.stage_name(stage.id), net);
      }
    }

    self.dup_labels = emitter.dup_labels;
  }

  pub fn emit_ivy(&mut self, value_def: &ValueDef<'core>) {
    let path = self.chart.defs[value_def.def].path;
    match &value_def.kind {
      ValueDefKind::Taken => unreachable!(),
      ValueDefKind::Const { .. } | ValueDefKind::Fn { .. } | ValueDefKind::TraitSubitem(..) => {}
      ValueDefKind::Ivy { net, .. } => {
        self.nets.insert(path.into(), net.clone());
      }
      ValueDefKind::Adt(adt_id, variant_id) => {
        let adt = &self.chart.adts[*adt_id];
        let fields =
          (0..adt.variants[*variant_id].fields.len()).map(|i| Tree::Var(format!("f{i}")));
        let root = Tree::n_ary(
          "fn",
          fields.clone().chain([make_adt(
            *variant_id,
            adt,
            || (Tree::Var("r".into()), Tree::Var("r".into())),
            fields,
          )]),
        );
        self.nets.insert(path.into(), Net { root, pairs: Vec::new() });
      }
    }
  }
}

struct VirEmitter<'core, 'a> {
  chart: &'a Chart<'core>,
  path: &'a str,
  stages: &'a IdxVec<StageId, Stage>,
  interfaces: &'a IdxVec<InterfaceId, Interface>,
  specs: &'a Specializations<'core>,
  spec: &'a Spec,
  locals: BTreeMap<Local, LocalState>,
  pairs: Vec<(Tree, Tree)>,
  wire_offset: usize,
  wires: Counter<usize>,
  dup_labels: Counter<usize>,
}

impl<'core, 'a> VirEmitter<'core, 'a> {
  fn dup_label(&mut self) -> String {
    format!("dup{}", self.dup_labels.next())
  }

  pub fn emit_transfer(&mut self, transfer: &Transfer) {
    let interface = &self.interfaces[transfer.interface];
    if interface.inline() {
      let InterfaceKind::Unconditional(stage) = interface.kind else { unreachable!() };
      return self.inline_stage(&self.stages[stage]);
    }

    let target = self.emit_interface(interface, false);

    self.pairs.push(match &interface.kind {
      InterfaceKind::Unconditional(stage) => (self.emit_stage_node(*stage), target),
      InterfaceKind::Branch([zero, non_zero]) => (
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
    });
  }

  fn finish_local(&mut self, mut local: LocalState) {
    if local.past.is_empty() {
      local.past.push((Vec::new(), Vec::new()));
    }
    let first = &mut local.past[0];
    first.0.append(&mut local.spaces);
    first.1.append(&mut local.values);
    for (mut spaces, mut values) in local.past.into_iter() {
      if spaces.is_empty() {
        for value in values {
          self.pairs.push((Tree::Erase, value));
        }
      } else if values.is_empty() {
        for space in spaces {
          self.pairs.push((space, Tree::Erase));
        }
      } else if values.len() == 1 {
        let label = self.dup_label();
        self.pairs.push((Tree::n_ary(&label, spaces), values.pop().unwrap()));
      } else if spaces.len() == 1 {
        let label = self.dup_label();
        self.pairs.push((spaces.pop().unwrap(), Tree::n_ary(&label, values)));
      } else {
        unreachable!()
      }
    }
  }

  fn inline_stage(&mut self, stage: &Stage) {
    let prev_wire_offset = self.wire_offset;
    self.wire_offset = self.wires.peek_next();
    self.wires.0 += stage.wires.0 .0;
    for local in &stage.declarations {
      if let Some(local) = self.locals.remove(local) {
        self.finish_local(local);
      }
    }
    for step in &stage.steps {
      self.emit_step(step);
    }
    self.wire_offset = prev_wire_offset;
  }

  fn _emit_stage(&mut self, stage: &Stage) {
    for local in &stage.declarations {
      if let Some(local) = self.locals.remove(local) {
        self.finish_local(local);
      }
    }
    for step in &stage.steps {
      self.emit_step(step);
    }
  }

  fn emit_interface(&mut self, interface: &Interface, side: bool) -> Tree {
    Tree::n_ary(
      "x",
      interface.wires.iter().filter_map(|(&local, usage)| {
        let usage = if side { usage.1 } else { usage.0 };
        match usage {
          Usage::Erase => {
            self.local(local).erase();
            None
          }
          Usage::Mut => {
            let a = self.new_wire();
            let b = self.new_wire();
            self.local(local).mutate(a.0, b.0);
            if side {
              Some(Tree::Comb("x".into(), Box::new(b.1), Box::new(a.1)))
            } else {
              Some(Tree::Comb("x".into(), Box::new(a.1), Box::new(b.1)))
            }
          }
          Usage::Set => {
            let w = self.new_wire();
            self.local(local).set(w.0);
            Some(w.1)
          }
          Usage::Take => {
            let w = self.new_wire();
            self.local(local).take(w.0);
            Some(w.1)
          }
          Usage::Get => {
            let w = self.new_wire();
            self.local(local).get(w.0);
            Some(w.1)
          }
          Usage::Hedge => {
            let w = self.new_wire();
            self.local(local).hedge(w.0);
            Some(w.1)
          }
          u => unreachable!("{u:?}"),
        }
      }),
    )
  }

  fn emit_stage_node(&self, stage_id: StageId) -> Tree {
    Tree::Global(self.stage_name(stage_id))
  }

  fn local(&mut self, local: Local) -> &mut LocalState {
    self.locals.entry(local).or_default()
  }

  fn emit_step(&mut self, step: &Step) {
    let wire_offset = self.wire_offset;
    let emit_port = |p| Self::_emit_port(wire_offset, self.chart, self.specs, self.spec, p);
    match step {
      Step::Invoke(local, invocation) => match invocation {
        Invocation::Erase => self.local(*local).erase(),
        Invocation::Get(port) => self.local(*local).get(emit_port(port)),
        Invocation::Hedge(port) => self.local(*local).hedge(emit_port(port)),
        Invocation::Take(port) => self.local(*local).take(emit_port(port)),
        Invocation::Set(port) => self.local(*local).set(emit_port(port)),
        Invocation::Mut(a, b) => self.local(*local).mutate(emit_port(a), emit_port(b)),
      },
      Step::Transfer(transfer) => self.emit_transfer(transfer),
      Step::Diverge(..) => unreachable!(),

      Step::Link(a, b) => self.pairs.push((emit_port(a), emit_port(b))),
      Step::Fn(func, args, ret) => self
        .pairs
        .push((emit_port(func), Tree::n_ary("fn", args.iter().chain([ret]).map(emit_port)))),
      Step::Tuple(port, tuple) => {
        self.pairs.push((emit_port(port), Tree::n_ary("tup", tuple.iter().map(emit_port))))
      }
      Step::Adt(adt_id, variant_id, port, fields) => {
        let adt = &self.chart.adts[*adt_id];
        let fields = fields.iter().map(emit_port);
        let adt = make_adt(*variant_id, adt, || self.new_wire(), fields);
        self.pairs.push((emit_port(port), adt));
      }
      Step::Ref(reference, value, space) => self.pairs.push((
        emit_port(reference),
        Tree::Comb("ref".into(), Box::new(emit_port(value)), Box::new(emit_port(space))),
      )),
      Step::ExtFn(ext_fn, swap, lhs, rhs, out) => self.pairs.push((
        emit_port(lhs),
        Tree::ExtFn(ext_fn.to_string(), *swap, Box::new(emit_port(rhs)), Box::new(emit_port(out))),
      )),
      Step::Dup(a, b, c) => {
        let label = self.dup_label();
        self
          .pairs
          .push((emit_port(a), Tree::Comb(label, Box::new(emit_port(b)), Box::new(emit_port(c)))))
      }
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
                Tree::ExtFn("add".into(), false, Box::new(cur_len), Box::new(next_len.0)),
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
    }
  }

  fn emit_port(&self, port: &Port) -> Tree {
    Self::_emit_port(self.wire_offset, self.chart, self.specs, self.spec, port)
  }

  fn _emit_port(
    wire_offset: usize,
    chart: &Chart,
    specs: &Specializations,
    spec: &Spec,
    port: &Port,
  ) -> Tree {
    match port {
      Port::Erase => Tree::Erase,
      Port::N32(n) => Tree::N32(*n),
      Port::F32(f) => Tree::F32(*f),
      Port::Wire(w) => Tree::Var(format!("w{}", wire_offset + w.0)),
      Port::Def(id) => Tree::Global(chart.defs[chart.values[*id].def].path.into()),
      Port::Rel(rel) => {
        let spec_id = spec.rels[*rel];
        let spec = specs.specs[spec_id].as_ref().unwrap();
        let path = chart.defs[chart.values[spec.value].def].path;
        Tree::Global(if spec.singular {
          path.to_string()
        } else {
          format!("{}::{}", path, spec.index)
        })
      }
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

  fn new_wire(&mut self) -> (Tree, Tree) {
    let label = format!("w{}", self.wires.next());
    (Tree::Var(label.clone()), Tree::Var(label))
  }

  fn stage_name(&self, stage_id: StageId) -> String {
    let mut name = self.path.to_owned();
    if !self.spec.singular {
      write!(name, "::{}", self.spec.index).unwrap();
    }
    if stage_id.0 != 0 {
      write!(name, "::{}", stage_id.0).unwrap();
    }
    name
  }
}

#[derive(Default)]
struct LocalState {
  past: Vec<(Vec<Tree>, Vec<Tree>)>,
  spaces: Vec<Tree>,
  values: Vec<Tree>,
}

impl LocalState {
  fn mutate(&mut self, a: Tree, b: Tree) {
    self.get(a);
    self.erase();
    self.hedge(b);
  }

  fn get(&mut self, port: Tree) {
    self.spaces.push(port);
  }

  fn take(&mut self, port: Tree) {
    self.get(port);
    self.erase();
  }

  fn hedge(&mut self, port: Tree) {
    self.values.push(port);
  }

  fn set(&mut self, port: Tree) {
    self.erase();
    self.hedge(port);
  }

  fn erase(&mut self) {
    if self.past.is_empty() || !self.spaces.is_empty() || !self.values.is_empty() {
      self.past.push((take(&mut self.spaces), take(&mut self.values)));
    }
  }
}

fn make_adt(
  variant_id: VariantId,
  adt: &AdtDef,
  mut new_wire: impl FnMut() -> (Tree, Tree),
  fields: impl DoubleEndedIterator<Item = Tree>,
) -> Tree {
  if adt.variants.len() == 1 {
    Tree::n_ary("tup", fields)
  } else {
    let wire = new_wire();
    let mut fields = Tree::n_ary("enum", fields.chain([wire.0]));
    Tree::n_ary(
      "enum",
      (0..adt.variants.len())
        .map(|i| if variant_id.0 == i { take(&mut fields) } else { Tree::Erase })
        .chain([wire.1]),
    )
  }
}
