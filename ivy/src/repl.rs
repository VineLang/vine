use std::fmt::{self, Display};

use indexmap::{map::Entry, IndexMap};
use ivm::{
  ext::ExtVal,
  global::Global,
  port::{Port, Tag},
  wire::Wire,
  IVM,
};
use vine_util::parser::{Parser, ParserState};

use crate::{
  ast::{Nets, Tree},
  parser::{IvyParser, ParseError},
  readback::Reader,
  serialize::Labels,
};

pub struct Repl<'a, 'l, 'ivm> {
  ivm: &'a mut IVM<'ivm>,
  nets: &'a Nets,
  globals: &'ivm [Global<'ivm>],
  labels: &'a mut Labels<'l>,

  vars: IndexMap<String, Port<'ivm>>,
}

impl<'a, 'l, 'ivm> Repl<'a, 'l, 'ivm> {
  pub fn new(
    ivm: &'a mut IVM<'ivm>,
    nets: &'a Nets,
    globals: &'ivm [Global<'ivm>],
    labels: &'a mut Labels<'l>,
  ) -> Self {
    let vars = [("io".to_owned(), Port::new_ext_val(ExtVal::IO))].into_iter().collect();
    Self { ivm, nets, globals, labels, vars }
  }

  pub fn exec<'s>(&mut self, line: &'s str) -> Result<(), ParseError<'s>> {
    let mut parser = IvyParser { state: ParserState::new(line) };
    parser.bump()?;
    let mut pairs = Vec::new();
    while parser.state.token.is_some() {
      pairs.push(parser.parse_pair()?);
    }
    for pair in pairs {
      self.exec_pair(pair);
    }
    Ok(())
  }

  pub fn exec_pair(&mut self, pair: (Tree, Tree)) {
    let p = self.inject(pair.0);
    let q = self.inject(pair.1);
    self.ivm.link(p, q);
    self.ivm.normalize();
  }

  fn inject_to(&mut self, tree: Tree, to: Wire<'ivm>) {
    if let Tree::Var(v) = tree {
      match self.vars.entry(v) {
        Entry::Occupied(e) => {
          let p = e.shift_remove();
          self.ivm.link_wire(to, p);
        }
        Entry::Vacant(e) => {
          e.insert(Port::new_wire(to));
        }
      }
    } else {
      let p = self.inject(tree);
      self.ivm.link_wire(to, p);
    }
  }

  fn inject(&mut self, tree: Tree) -> Port<'ivm> {
    match tree {
      Tree::Erase => Port::ERASE,
      Tree::U32(value) => Port::new_ext_val(ExtVal::new_u32(value)),
      Tree::F32(value) => Port::new_ext_val(ExtVal::new_f32(value)),
      Tree::Global(name) => Port::new_global(&self.globals[self.nets.get_index_of(&name).unwrap()]),
      Tree::Comb(label, a, b) => {
        let label = self.labels.to_id(label);
        let n = unsafe { self.ivm.new_node(Tag::Comb, label) };
        self.inject_to(*a, n.1);
        self.inject_to(*b, n.2);
        n.0
      }
      Tree::ExtFn(f, a, b) => {
        let n = unsafe { self.ivm.new_node(Tag::ExtFn, f.bits()) };
        self.inject_to(*a, n.1);
        self.inject_to(*b, n.2);
        n.0
      }
      Tree::Branch(z, p, o) => {
        let n = unsafe { self.ivm.new_node(Tag::Branch, 0) };
        let m = unsafe { self.ivm.new_node(Tag::Branch, 0) };
        self.ivm.link_wire(n.1, m.0);
        self.inject_to(*z, m.1);
        self.inject_to(*p, m.2);
        self.inject_to(*o, n.2);
        n.0
      }
      Tree::Var(v) => match self.vars.entry(v) {
        Entry::Occupied(e) => e.shift_remove(),
        Entry::Vacant(e) => {
          let (a, b) = self.ivm.new_wire();
          e.insert(Port::new_wire(a));
          Port::new_wire(b)
        }
      },
    }
  }
}

impl Display for Repl<'_, '_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut reader = Reader::new(self.ivm, self.labels);
    for (var, port) in &self.vars {
      writeln!(f, "{} = {}", var, reader.read_port(port))?;
    }
    Ok(())
  }
}
