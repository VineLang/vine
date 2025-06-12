use std::{
  collections::{hash_map::Entry, HashMap},
  mem::take,
};

use ivm::{ext::Extrinsics, heap::Heap, port::Port, IVM};

use crate::{
  ast::{Net, Nets, Tree},
  host::Host,
};

pub fn pre_reduce(nets: &mut Nets) {
  let mut prune = PreReduce { nets, marks: HashMap::new() };
  prune.visit_global("::");
  let marks = prune.marks;

  let heap = &Heap::new();
  let mut host = &mut Host::default();
  let mut extrinsics = Extrinsics::default();
  host.register_default_extrinsics(&mut extrinsics);
  host._insert_nets(nets, true);

  for (name, net) in nets.iter_mut() {
    if marks.get(name) != Some(&true) {
      continue;
    }
    let ivm = &mut IVM::new(heap, &extrinsics);
    let r = ivm.new_wire();
    ivm.execute(&host.get(name).unwrap().instructions, Port::new_wire(r.0));
    ivm.normalize();
    let inert = take(&mut ivm.inert);
    let mut reader = host.reader(ivm);
    net.root = reader.read_port(&Port::new_wire(r.1));
    net.pairs.clear();
    net.pairs.extend(
      inert
        .into_iter()
        .map(|(a, b)| (reader.read_port(&a), Tree::BlackBox(Box::new(reader.read_port(&b))))),
    );
  }
}

struct PreReduce<'a> {
  nets: &'a Nets,
  marks: HashMap<String, bool>,
}

impl PreReduce<'_> {
  fn visit_global(&mut self, name: &str) {
    match self.marks.entry(name.to_owned()) {
      Entry::Vacant(e) => {
        e.insert(false);
        self.visit_net(&self.nets[name]);
      }
      Entry::Occupied(mut e) => {
        let marked = e.get_mut();
        if !*marked {
          *marked = true;
          self.visit_net(&self.nets[name]);
        }
      }
    }
  }

  fn visit_net(&mut self, net: &Net) {
    for tree in net.trees() {
      self.visit_tree(tree);
    }
  }

  fn visit_tree(&mut self, tree: &Tree) {
    if let Tree::Global(name) = tree {
      self.visit_global(name);
    }
    for child in tree.children() {
      self.visit_tree(child);
    }
  }
}
