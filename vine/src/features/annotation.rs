use crate::{
  components::resolver::Resolver,
  structures::{
    ast::{Pat, Span, Ty},
    diag::Diag,
    tir::TirPat,
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_pat_annotation(
    &mut self,
    span: Span,
    pat: &Pat<'core>,
    ty: &Ty<'core>,
  ) -> Result<TirPat, Diag<'core>> {
    let ty = self.resolve_ty(ty, true);
    Ok(TirPat::new(span, ty, *self.resolve_pat_type(pat, ty).kind))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_pat_annotation(&self, pat: &Pat<'core>, ty: &Ty<'core>) -> Doc<'src> {
    Doc::concat([self.fmt_pat(pat), Doc(": "), self.fmt_ty(ty)])
  }
}
