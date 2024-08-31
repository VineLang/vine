use crate::ast::{Ident, Path};

use super::{NodeId, Resolver};

impl Resolver {
  pub fn resolve_path(&mut self, base: NodeId, path: &Path) -> NodeId {
    self
      .try_resolve_path(base, path)
      .unwrap_or_else(|| panic!("cannot resolve {path} in {}", self.nodes[base].canonical))
  }

  pub fn try_resolve_path(&mut self, base: NodeId, path: &Path) -> Option<NodeId> {
    let base = if path.absolute { 0 } else { base };
    let mut check_parents = true;
    path.segments.iter().try_fold(base, |b, &s| {
      let next = self.try_resolve_one(b, s, check_parents);
      check_parents = false;
      next
    })
  }

  fn try_resolve_one(
    &mut self,
    base: NodeId,
    segment: Ident,
    check_parents: bool,
  ) -> Option<NodeId> {
    let node = &mut self.nodes[base];

    if let Some(&result) = node.children.get(&segment) {
      return Some(result);
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
      return Some(resolved);
    }

    if check_parents {
      if let Some(parent) = node.parent {
        return self.try_resolve_one(parent, segment, true);
      }
    }

    None
  }
}
