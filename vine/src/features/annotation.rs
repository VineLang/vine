use crate::{
  components::resolver::Resolver,
  structures::{
    ast::{Pat, Span, Ty},
    content::{Content, Punct, Space},
    diag::Diag,
    tir::TirPat,
  },
  tools::fmt::Formatter,
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
  pub(crate) fn fmt_pat_annotation(&self, pat: &Pat, ty: &Ty) -> Content {
    Content::smart((self.fmt_pat(pat), Punct(":"), Space, self.fmt_ty(ty)))
  }
}
