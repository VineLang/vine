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
  let mut seen = HashSet::new();
  walk(table, nets, entry, |name| seen.insert(name));
  if !seen.is_empty() {
    nets.retain(|name, _| seen.contains(name));
  }
}

fn walk(
  table: &Table,
  nets: &HashMap<NameId, FlatNet>,
  names: impl IntoIterator<Item = NameId>,
  mut f: impl FnMut(NameId) -> bool,
) {
  for name in names {
    visit(table, nets, name, &mut f);
  }

  fn visit(
    table: &Table,
    nets: &HashMap<NameId, FlatNet>,
    name: NameId,
    f: &mut impl FnMut(NameId) -> bool,
  ) {
    if f(name) {
      for &child in &table.name(name).children {
        visit(table, nets, child, f);
      }
      if let Some(net) = nets.get(&name) {
        for node in &net.nodes {
          if let Some(name) = table.has_name(&node.name) {
            visit(table, nets, name, f);
          } else {
            for &child in &node.name.children {
              visit(table, nets, child, f);
            }
          }
        }
      }
    }
  }
}
