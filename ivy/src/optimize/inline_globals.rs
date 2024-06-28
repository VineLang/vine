use std::collections::HashMap;

use crate::ast::{Net, Nets, Tree};

pub fn inline_globals(nets: &mut Nets) {
  let mut inliner = Inliner::default();
  inliner.populate_candidates(nets);
  for (_, net) in nets.iter_mut() {
    for tree in net.trees_mut() {
      inliner.process(tree);
    }
  }
}

#[derive(Debug, Default)]
struct Inliner {
  candidates: HashMap<String, Tree>,
}

impl Inliner {
  fn populate_candidates(&mut self, nets: &Nets) {
    for (name, mut net) in nets.iter() {
      if net.should_inline() {
        while let Tree::Global(name) = &net.root {
          let referenced = &nets[name];
          if referenced.should_inline() {
            net = referenced;
          } else {
            break;
          }
        }
        self.candidates.insert(name.to_owned(), net.root.clone());
      }
    }
  }

  fn process(&self, tree: &mut Tree) {
    if let Tree::Global(name) = tree {
      if let Some(inlined) = self.candidates.get(name) {
        *tree = inlined.clone();
      }
    } else {
      for t in tree.children_mut() {
        self.process(t);
      }
    }
  }
}

impl Net {
  fn should_inline(&self) -> bool {
    self.pairs.is_empty() && self.root.children().next().is_none()
  }
}