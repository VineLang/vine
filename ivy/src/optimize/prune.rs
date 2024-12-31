use std::collections::HashSet;

use crate::ast::{Net, Nets, Tree};

/// Prune unused global nets.
pub fn prune(nets: &mut Nets) {
  if nets.contains_key("::main") {
    let mut prune = Prune { nets, keep: HashSet::new() };
    prune.visit_global("::main");
    let keep = prune.keep;
    nets.retain(|name, _| keep.contains(name));
  }
}

struct Prune<'a> {
  nets: &'a Nets,
  keep: HashSet<String>,
}

impl Prune<'_> {
  fn visit_global(&mut self, name: &str) {
    if self.keep.insert(name.to_owned()) {
      self.visit_net(&self.nets[name]);
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
