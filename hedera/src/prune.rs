use std::collections::{HashMap, HashSet};

use crate::{
  name::{NameId, Table},
  net::FlatNet,
};

pub fn prune(
  table: &Table,
  nets: &mut HashMap<NameId, FlatNet>,
  entry: impl IntoIterator<Item = NameId>,
) {
  let mut walker = Walker { table, nets, seen: HashSet::new() };
  for name in entry {
    walker.visit_name(name);
  }
  if !walker.seen.is_empty() {
    nets.retain(|name, _| walker.seen.contains(name));
  }
}

struct Walker<'a> {
  table: &'a Table,
  nets: &'a HashMap<NameId, FlatNet>,
  seen: HashSet<NameId>,
}

impl Walker<'_> {
  fn visit_name(&mut self, name: NameId) {
    if self.seen.insert(name) {
      for &child in &self.table.name(name).children {
        self.visit_name(child);
      }
      if let Some(net) = self.nets.get(&name) {
        for node in &net.nodes {
          if let Some(name) = self.table.has_name(&node.name) {
            self.visit_name(name);
          } else {
            for &child in &node.name.children {
              self.visit_name(child);
            }
          }
        }
      }
    }
  }
}
