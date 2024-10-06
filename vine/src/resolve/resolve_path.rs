use crate::{
  ast::{Ident, Path},
  diag::Diag,
};

use super::{Member, NodeId, Resolver};

impl Resolver {
  pub fn resolve_path(&mut self, base: NodeId, path: &Path) -> Result<NodeId, Diag> {
    let base = if path.absolute { 0 } else { base };
    let mut check_parents = true;
    let mut cur = base;
    for &segment in &path.segments {
      cur = self.resolve_one(cur, segment, check_parents).ok_or_else(|| Diag::CannotResolve {
        span: path.span,
        name: segment,
        module: self.nodes[cur].canonical.clone(),
      })?;
      check_parents = false;
    }
    Ok(cur)
  }

  pub(super) fn resolve_one(
    &mut self,
    base: NodeId,
    segment: Ident,
    check_parents: bool,
  ) -> Option<NodeId> {
    let node = &mut self.nodes[base];

    if let Some(member) = node.members.get_mut(&segment) {
      return match member {
        Member::Child(result) | Member::ResolvedImport(result, _) => Some(*result),
        Member::UnresolvedImport(import, id) => {
          let path = import.take()?;
          let id = *id;
          match self.resolve_path(base, &path) {
            Ok(resolved) => {
              let node = &mut self.nodes[base];
              node.members.insert(segment, Member::ResolvedImport(resolved, id));
              Some(resolved)
            }
            Err(diag) => {
              self.diags.add(diag);
              None
            }
          }
        }
      };
    }

    if check_parents {
      if let Some(parent) = node.parent {
        return self.resolve_one(parent, segment, true);
      }
    }

    None
  }

  pub fn resolve_imports(&mut self) {
    self._resolve_imports(0..self.nodes.len());
  }

  pub(crate) fn _resolve_imports(&mut self, nodes: impl IntoIterator<Item = NodeId>) {
    let mut unresolved_imports = Vec::new();
    for node in nodes {
      unresolved_imports.extend(
        self.nodes[node]
          .members
          .iter()
          .filter(|x| matches!(x.1, Member::UnresolvedImport(..)))
          .map(|x| *x.0),
      );
      for name in unresolved_imports.drain(..) {
        self.resolve_one(node, name, false);
      }
    }
  }
}
