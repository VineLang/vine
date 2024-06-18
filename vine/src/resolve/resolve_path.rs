use crate::ast::{Ident, Path};

use super::{NodeId, Resolver};

impl Resolver {
  pub fn resolve_path(&mut self, base: NodeId, path: &Path) -> NodeId {
    path.segments.iter().fold(base, |b, &s| self.resolve_one(b, s))
  }

  fn resolve_one(&mut self, base: NodeId, segment: Ident) -> NodeId {
    let node = &mut self.nodes[base];

    if let Some(&result) = node.children.get(&segment) {
      return result;
    }

    if let Some(import) = node.imports.get_mut(&segment) {
      let import = import.take();
      let Some(path) = import else {
        panic!("circular import");
      };
      let resolved = self.resolve_path(base, &path);
      let node = &mut self.nodes[base];
      node.imports.remove(&segment);
      node.children.insert(segment, resolved);
      return resolved;
    }

    if let Some(parent) = node.parent {
      return self.resolve_one(parent, segment);
    }

    panic!("cannot resolve {:?} in {:?}", segment, node.canonical);
  }
}
