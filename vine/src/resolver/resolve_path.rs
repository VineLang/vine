use crate::{
  ast::{Ident, Path, Span},
  chart::{Def, DefId, ImportDef, ImportId, ImportParent, MemberKind, WithVis},
  diag::{Diag, ErrorGuaranteed},
  signatures::ImportState,
};

use super::Resolver;

impl<'core> Resolver<'core, '_> {
  pub fn resolve_path_to<T>(
    &mut self,
    base: DefId,
    path: &Path<'core>,
    desc: &'static str,
    f: impl FnOnce(&Def) -> Option<WithVis<T>>,
  ) -> Result<T, Diag<'core>> {
    let def = self.resolve_path(base, path)?;
    let def = &self.chart.defs[def];
    match f(def) {
      Some(WithVis { vis, kind }) => {
        if self.chart.visible(vis, base) {
          Ok(kind)
        } else {
          Err(Diag::InvisibleAssociated {
            span: path.span,
            desc,
            path: def.path,
            vis: self.chart.defs[vis].path,
          })
        }
      }
      None => Err(Diag::PathNoAssociated { span: path.span, desc, path: def.path }),
    }
  }

  pub fn resolve_path(&mut self, base: DefId, path: &Path<'core>) -> Result<DefId, Diag<'core>> {
    if path.absolute {
      self._resolve_path(path.span, base, DefId::ROOT, &path.segments)
    } else {
      let initial = *path.segments.first().unwrap();
      let initial = self.resolve_initial(path.span, base, initial)?;
      self._resolve_path(path.span, base, initial, &path.segments[1..])
    }
  }

  pub(super) fn _resolve_path(
    &mut self,
    span: Span,
    source: DefId,
    base: DefId,
    segments: &[Ident<'core>],
  ) -> Result<DefId, Diag<'core>> {
    let mut cur = base;
    for &ident in segments {
      cur = self.resolve_ident(span, source, cur, ident)?;
    }
    Ok(cur)
  }

  fn resolve_initial(
    &mut self,
    span: Span,
    base: DefId,
    ident: Ident<'core>,
  ) -> Result<DefId, Diag<'core>> {
    let mut cur = base;
    loop {
      if let Some(resolved) = self._resolve_ident(span, base, cur, ident)? {
        return Ok(resolved);
      }
      if let Some(parent) = self.chart.defs[cur].parent {
        cur = parent;
      } else {
        break;
      }
    }
    if let Some(prelude) = self.chart.builtins.prelude {
      if let Some(resolved) = self._resolve_ident(span, base, prelude, ident)? {
        return Ok(resolved);
      }
    }
    Err(Diag::CannotResolve { span, module: self.chart.defs[base].path, ident })
  }

  pub fn resolve_ident(
    &mut self,
    span: Span,
    source: DefId,
    base: DefId,
    ident: Ident<'core>,
  ) -> Result<DefId, Diag<'core>> {
    let resolved = self._resolve_ident(span, source, base, ident)?;
    resolved.ok_or(Diag::CannotResolve { span, module: self.chart.defs[base].path, ident })
  }

  fn _resolve_ident(
    &mut self,
    span: Span,
    source: DefId,
    base: DefId,
    ident: Ident<'core>,
  ) -> Result<Option<DefId>, Diag<'core>> {
    let def = &self.chart.defs[base];

    if let Some(member) = def.members.get(&ident) {
      let vis = member.vis;
      let result = match member.kind {
        MemberKind::Child(result) => result,
        MemberKind::Import(import) => self.resolve_import(import)?,
      };
      if self.chart.visible(vis, source) {
        Ok(Some(result))
      } else {
        Err(Diag::Invisible {
          span,
          module: self.chart.defs[base].path,
          ident,
          vis: self.chart.defs[vis].path,
        })
      }
    } else {
      Ok(None)
    }
  }

  pub(super) fn resolve_import(&mut self, import_id: ImportId) -> Result<DefId, ErrorGuaranteed> {
    let import = &self.chart.imports[import_id];
    let state = self.sigs.imports.get_or_extend_with(import_id, || ImportState::Unresolved);
    match state {
      ImportState::Resolved(resolved) => *resolved,
      ImportState::Resolving => Err(self.core.report(Diag::CircularImport { span: import.span })),
      ImportState::Unresolved => {
        *state = ImportState::Resolving;
        let import = *import;
        let resolved = self._resolve_import(import);
        self.sigs.imports[import_id] = ImportState::Resolved(resolved);
        resolved
      }
    }
  }

  fn _resolve_import(&mut self, import: ImportDef<'core>) -> Result<DefId, ErrorGuaranteed> {
    let ImportDef { span, def, parent, ident, .. } = import;
    match parent {
      ImportParent::Root => self.resolve_ident(span, def, DefId::ROOT, ident),
      ImportParent::Scope => self.resolve_initial(span, def, ident),
      ImportParent::Import(parent) => {
        let parent = self.resolve_import(parent)?;
        self.resolve_ident(span, def, parent, ident)
      }
    }
    .map_err(|diag| self.core.report(diag))
  }
}
