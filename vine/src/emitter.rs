use std::{collections::BTreeMap, mem::take};

use ivy::ast::Tree;
use vine_util::idx::{Counter, IdxVec};

use crate::{
  analyzer::usage::Usage,
  ast::{Local, Path},
  resolver::Resolver,
  vir::{Interface, InterfaceId, InterfaceKind, Invocation, Port, Stage, StageId, Step, Transfer},
};

pub struct Emitter<'core, 'a> {
  resolver: &'a Resolver<'core>,
  path: &'a Path<'core>,
  stages: &'a IdxVec<StageId, Stage>,
  interfaces: &'a IdxVec<InterfaceId, Interface>,
  locals: BTreeMap<Local, LocalState>,
  pairs: Vec<(Tree, Tree)>,
  wire_offset: usize,
  wires: Counter<usize>,
  dup_labels: Counter<usize>,
}

impl<'core, 'a> Emitter<'core, 'a> {
  fn dup_label(&mut self) -> String {
    format!("dup{}", self.dup_labels.next())
  }

  pub fn emit_transfer(&mut self, transfer: &Transfer) {
    let interface = &self.interfaces[transfer.interface];
    if interface.inline() {
      let InterfaceKind::Unconditional(stage) = interface.kind else { unreachable!() };
      return self._emit_stage(&self.stages[stage]);
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
        Self::emit_n_ary("enum", stages.iter().map(|&s| self.emit_stage_node(s)).chain([target])),
      ),
    });
  }

  fn finish_local(&mut self, mut local: LocalState) {
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
        self.pairs.push((Self::emit_n_ary(&label, spaces), values.pop().unwrap()));
      } else if spaces.len() == 1 {
        let label = self.dup_label();
        self.pairs.push((spaces.pop().unwrap(), Self::emit_n_ary(&label, values)));
      } else {
        unreachable!()
      }
    }
  }

  fn _emit_stage(&mut self, stage: &Stage) {
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

  fn emit_interface(&mut self, interface: &Interface, side: bool) -> Tree {
    Self::emit_n_ary(
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
            Some(Tree::Comb("x".into(), Box::new(a.1), Box::new(b.1)))
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
          _ => unreachable!(),
        }
      }),
    )
  }

  fn emit_stage_node(&self, stage: StageId) -> Tree {
    Tree::Global(format!("{}::{}", self.path, stage.0))
  }

  fn local(&mut self, local: Local) -> &mut LocalState {
    self.locals.entry(local).or_default()
  }

  fn emit_step(&mut self, step: &Step) {
    let wire_offset = self.wire_offset;
    let emit_port = |p| Self::_emit_port(wire_offset, self.resolver, p);
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
        .push((emit_port(func), Self::emit_n_ary("fn", args.iter().chain([ret]).map(emit_port)))),
      Step::Tuple(port, tuple) => {
        self.pairs.push((emit_port(port), Self::emit_n_ary("tup", tuple.iter().map(emit_port))))
      }
      Step::Adt(variant, port, fields) => {
        let variant = self.resolver.defs[*variant].variant_def.as_ref().unwrap();
        let adt = self.resolver.defs[variant.adt].adt_def.as_ref().unwrap();
        let adt = if adt.variants.len() == 1 {
          Self::emit_n_ary("tup", fields.iter().map(emit_port))
        } else {
          let wire = self.new_wire();
          let mut fields = Self::emit_n_ary("tup", fields.iter().map(emit_port).chain([wire.0]));
          Self::emit_n_ary(
            "enum",
            (0..adt.variants.len())
              .map(|i| if variant.variant == i { take(&mut fields) } else { Tree::Erase })
              .chain([wire.1]),
          )
        };
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
    Self::_emit_port(self.wire_offset, self.resolver, port)
  }

  fn _emit_port(wire_offset: usize, resolver: &Resolver, port: &Port) -> Tree {
    match port {
      Port::Erase => Tree::Erase,
      Port::N32(n) => Tree::N32(*n),
      Port::F32(f) => Tree::F32(*f),
      Port::Wire(w) => Tree::Var(format!("w{}", wire_offset + w.0)),
      Port::Const(def) => Tree::Global(resolver.defs[*def].canonical.to_string()),
    }
  }

  fn emit_list(
    &mut self,
    ports: impl IntoIterator<IntoIter: DoubleEndedIterator<Item = Tree>>,
  ) -> Tree {
    let end = self.new_wire();
    let mut len = 0;
    let buf = Self::emit_n_ary("tup", ports.into_iter().inspect(|_| len += 1).chain([end.0]));
    Self::emit_n_ary("tup", [Tree::N32(len), buf, end.1])
  }

  fn emit_n_ary(
    label: &str,
    ports: impl IntoIterator<IntoIter: DoubleEndedIterator<Item = Tree>>,
  ) -> Tree {
    ports
      .into_iter()
      .rev()
      .reduce(|b, a| Tree::Comb(label.into(), Box::new(a), Box::new(b)))
      .unwrap_or(Tree::Erase)
  }

  fn new_wire(&mut self) -> (Tree, Tree) {
    let label = format!("w{}", self.wires.next());
    (Tree::Var(label.clone()), Tree::Var(label))
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
