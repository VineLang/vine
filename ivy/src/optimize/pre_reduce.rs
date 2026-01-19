use std::{
  collections::{HashMap, hash_map::Entry},
  mem::take,
};

use ivm::{IVM, ext::Extrinsics, heap::Heap, port::Port};

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
    let inert_links = take(&mut ivm.inert_links);
    let inert_nodes = take(&mut ivm.inert_nodes);
    let mut reader = host.reader(ivm);
    net.root = reader.read_port(&Port::new_wire(r.1));
    net.pairs.clear();
    net.pairs.extend(
      inert_links
        .into_iter()
        .map(|(a, b)| (reader.read_port(&a), Tree::BlackBox(Box::new(reader.read_port(&b))))),
    );
    net.pairs.extend(inert_nodes.into_iter().map(|(ext_fn, a, b, c)| {
      (
        reader.read_port(&a),
        Tree::ExtFn(
          Host::label_from_u16(ext_fn.label, &host.opaque_ext_fn_labels).to_owned(),
          ext_fn.swap,
          Box::new(reader.read_port(&b)),
          Box::new(reader.read_port(&c)),
        ),
      )
    }));
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
