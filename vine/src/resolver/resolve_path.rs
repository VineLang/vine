use vine_util::idx::RangeExt;

use crate::{
  ast::{Ident, Path, Span},
  diag::Diag,
};

use super::{DefId, MemberKind, Resolver};

impl<'core> Resolver<'core> {
  pub fn resolve_path(
    &mut self,
    span: Span,
    from: DefId,
    base: DefId,
    path: &Path<'core>,
  ) -> Result<DefId, Diag<'core>> {
    let base = if path.absolute { DefId::ROOT } else { base };
    let mut cur = base;
    for &segment in &path.segments {
      let (base, vis, resolved) =
        self.resolve_one(cur, segment, from == base).ok_or_else(|| Diag::CannotResolve {
          span,
          name: segment,
          module: self.defs[cur].canonical.clone(),
        })?;
      if !self.visible(vis, from) {
        let mut path = self.defs[base].canonical.clone();
        path.segments.push(segment);
        return Err(Diag::Invisible { span, path, vis: self.defs[vis].canonical.clone() });
      }
      cur = resolved;
    }
    Ok(cur)
  }

  fn resolve_one(
    &mut self,
    base: DefId,
    segment: Ident<'core>,
    check_parents: bool,
  ) -> Option<(DefId, DefId, DefId)> {
    let def = &mut self.defs[base];

    if let Some(member) = def.members.get_mut(&segment) {
      let vis = member.vis;
      return match &mut member.kind {
        MemberKind::Child(result) | MemberKind::ResolvedImport(result, _) => {
          Some((base, vis, *result))
        }
        MemberKind::UnresolvedImport(span, import, id) => {
          let span = *span;
          let path = import.take()?;
          let id = *id;
          match self.resolve_path(span, base, base, &path) {
            Ok(resolved) => {
              let def = &mut self.defs[base];
              def.members.get_mut(&segment).unwrap().kind =
                MemberKind::ResolvedImport(resolved, id);
              Some((base, vis, resolved))
            }
            Err(diag) => {
              self.core.report(diag);
              None
            }
          }
        }
      };
    }

    if check_parents {
      if let Some(parent) = def.parent {
        return self.resolve_one(parent, segment, true);
      }
    }

    None
  }

  pub fn resolve_imports(&mut self) {
    self._resolve_imports(self.defs.range().iter());
  }

  pub(crate) fn _resolve_imports(&mut self, defs: impl IntoIterator<Item = DefId>) {
    let mut unresolved_imports = Vec::new();
    for def in defs {
      unresolved_imports.extend(
        self.defs[def]
          .members
          .iter()
          .filter(|x| matches!(x.1.kind, MemberKind::UnresolvedImport(..)))
          .map(|x| *x.0),
      );
      unresolved_imports.sort();
      for name in unresolved_imports.drain(..) {
        self.resolve_one(def, name, false);
      }
    }
  }

  pub(crate) fn visible(&self, vis: DefId, from: DefId) -> bool {
    if vis == from {
      return true;
    }
    if vis > from {
      return false;
    }
    for &ancestor in self.defs[from].ancestors.iter() {
      if ancestor == vis {
        return true;
      }
      if ancestor > vis {
        return false;
      }
    }
    false
  }
}
