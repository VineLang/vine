use crate::{
  components::resolver::Resolver,
  structures::{
    ast::{Pat, Span, Ty},
    diag::Diag,
    tir::TirPat,
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Resolver<'_> {
  pub(crate) fn resolve_pat_annotation(
    &mut self,
    span: Span,
    pat: &Pat,
    ty: &Ty,
  ) -> Result<TirPat, Diag> {
    let ty = self.resolve_ty(ty, true);
    Ok(TirPat::new(span, ty, *self.resolve_pat_type(pat, ty).kind))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_pat_annotation(&self, pat: &Pat, ty: &Ty) -> Doc<'src> {
    Doc::concat([self.fmt_pat(pat), Doc(": "), self.fmt_ty(ty)])
  }
}
