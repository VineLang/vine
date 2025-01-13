use std::{collections::BTreeMap, fmt::Write, mem::take};

use ivy::ast::{Net, Nets, Tree};
use vine_util::idx::{Counter, IdxVec};

use crate::{
  analyzer::usage::Usage,
  ast::Local,
  resolver::{AdtDef, Def, Resolver, ValueDefKind, VariantDef},
  specializer::{Spec, SpecId},
  vir::{
    Interface, InterfaceId, InterfaceKind, Invocation, Port, Stage, StageId, Step, Transfer, VIR,
  },
};

pub struct Emitter<'core, 'a> {
  pub nets: Nets,
  resolver: &'a Resolver<'core>,
  dup_labels: Counter<usize>,
}

impl<'core, 'a> Emitter<'core, 'a> {
  pub fn new(resolver: &'a Resolver<'core>) -> Self {
    Emitter { nets: Nets::default(), resolver, dup_labels: Counter::default() }
  }

  pub fn emit_vir(&mut self, path: String, vir: &VIR, specs: &IdxVec<SpecId, Spec>) {
    let mut emitter = VirEmitter {
      resolver: self.resolver,
      path,
      stages: &vir.stages,
      interfaces: &vir.interfaces,
      specs,
      spec_id: SpecId::default(),
      locals: BTreeMap::new(),
      pairs: Vec::new(),
      wire_offset: 0,
      wires: Counter::default(),
      dup_labels: self.dup_labels,
    };

    for spec_id in specs.keys() {
      emitter.spec_id = spec_id;
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
    }

    self.dup_labels = emitter.dup_labels;
  }

  pub fn emit_ivy(&mut self, def: &Def<'core>) {
    if let Some(value_def) = &def.value_def {
      match &value_def.kind {
        ValueDefKind::Expr(_) | ValueDefKind::TraitSubitem(..) => {}
        ValueDefKind::Ivy(net) => {
          self.nets.insert(def.canonical.to_string(), net.clone());
        }
        ValueDefKind::AdtConstructor => {
          let variant = def.variant_def.as_ref().unwrap();
          let adt = self.resolver.defs[variant.adt].adt_def.as_ref().unwrap();
          let fields = (0..variant.fields.len()).map(|i| Tree::Var(format!("f{i}")));
          let root = Tree::n_ary(
            "fn",
            fields.clone().chain([make_adt(
              variant,
              adt,
              || (Tree::Var("r".into()), Tree::Var("r".into())),
              fields,
            )]),
          );
          self.nets.insert(def.canonical.to_string(), Net { root, pairs: Vec::new() });
        }
      }
    }
  }
}

struct VirEmitter<'core, 'a> {
  resolver: &'a Resolver<'core>,
  path: String,
  stages: &'a IdxVec<StageId, Stage>,
  interfaces: &'a IdxVec<InterfaceId, Interface>,
  specs: &'a IdxVec<SpecId, Spec>,
  spec_id: SpecId,
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
    let spec_id = self.spec_id;
    let emit_port = |p| Self::_emit_port(wire_offset, self.resolver, &self.specs[spec_id], p);
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
      Step::Adt(variant, port, fields) => {
        let variant = self.resolver.defs[*variant].variant_def.as_ref().unwrap();
        let adt = self.resolver.defs[variant.adt].adt_def.as_ref().unwrap();
        let fields = fields.iter().map(emit_port);
        let adt = make_adt(variant, adt, || self.new_wire(), fields);
        self.pairs.push((emit_port(port), adt));
      }
      Step::Ref(reference, value, space) => self.pairs.push((
        emit_port(reference),
        Tree::Comb("ref".into(), Box::new(emit_port(value)), Box::new(emit_port(space))),
      )),
      Step::ExtFn(ext_fn, lhs, rhs, out) => self.pairs.push((
        emit_port(lhs),
        Tree::ExtFn(*ext_fn, Box::new(emit_port(rhs)), Box::new(emit_port(out))),
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
      Step::String(port, str) => {
        let str = self.emit_list(str.chars().map(|c| Tree::N32(c as u32)));
        self.pairs.push((emit_port(port), str))
      }
    }
  }

  fn emit_port(&self, port: &Port) -> Tree {
    Self::_emit_port(self.wire_offset, self.resolver, &self.specs[self.spec_id], port)
  }

  fn _emit_port(wire_offset: usize, resolver: &Resolver, spec: &Spec, port: &Port) -> Tree {
    match port {
      Port::Erase => Tree::Erase,
      Port::N32(n) => Tree::N32(*n),
      Port::F32(f) => Tree::F32(*f),
      Port::Wire(w) => Tree::Var(format!("w{}", wire_offset + w.0)),
      Port::Const(def) => Tree::Global(resolver.defs[*def].canonical.to_string()),
      Port::Rel(rel) => {
        let (def, spec, singular) = spec.rels[*rel];
        let path = &resolver.defs[def].canonical;
        Tree::Global(if singular { path.to_string() } else { format!("{}::{}", path, spec.0) })
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
    let mut name = self.path.clone();
    if !self.specs[self.spec_id].singular {
      write!(name, "::{}", self.spec_id.0).unwrap();
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
  variant: &VariantDef,
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
        .map(|i| if variant.variant == i { take(&mut fields) } else { Tree::Erase })
        .chain([wire.1]),
    )
  }
}
