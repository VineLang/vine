use crate::{
  components::{
    distiller::{Distiller, Poly},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, Pat, Span, Ty},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::Type,
    vir::{Port, Stage},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_inverse(&self, expr: &Expr, postfix: bool) -> Doc<'src> {
    if postfix {
      Doc::concat([self.fmt_expr(expr), Doc(".~")])
    } else {
      Doc::concat([Doc("~"), self.fmt_expr(expr)])
    }
  }

  pub(crate) fn fmt_pat_inverse(&self, pat: &Pat) -> Doc<'src> {
    Doc::concat([Doc("~"), self.fmt_pat(pat)])
  }

  pub(crate) fn fmt_ty_inverse(&self, ty: &Ty) -> Doc<'src> {
    Doc::concat([Doc("~"), self.fmt_ty(ty)])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_inverse(&mut self, span: Span, inner: &Expr) -> Result<TirExpr, Diag> {
    let inner = self.resolve_expr(inner);
    Ok(TirExpr::new(span, inner.ty.inverse(), TirExprKind::Inverse(inner)))
  }

  pub(crate) fn resolve_pat_inverse(&mut self, span: Span, inner: &Pat) -> Result<TirPat, Diag> {
    let inner = self.resolve_pat(inner);
    Ok(TirPat::new(span, inner.ty.inverse(), TirPatKind::Inverse(inner)))
  }

  pub(crate) fn resolve_pat_sig_inverse(&mut self, inner: &Pat, inference: bool) -> Type {
    let inner = self.resolve_pat_sig(inner, inference);
    inner.inverse()
  }

  pub(crate) fn resolve_ty_inverse(&mut self, inner: &Ty, inference: bool) -> Type {
    let inner = self.resolve_ty(inner, inference);
    inner.inverse()
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_inverse(&mut self, stage: &mut Stage, inner: &TirExpr) -> Port {
    self.distill_expr_space(stage, inner)
  }

  pub(crate) fn distill_expr_space_inverse(&mut self, stage: &mut Stage, inner: &TirExpr) -> Port {
    self.distill_expr_value(stage, inner)
  }

  pub(crate) fn distill_expr_place_inverse(
    &mut self,
    stage: &mut Stage,
    inner: &TirExpr,
  ) -> (Port, Port) {
    let (value, space) = self.distill_expr_place(stage, inner);
    (space, value)
  }

  pub(crate) fn distill_expr_poly_inverse(&mut self, stage: &mut Stage, inner: &TirExpr) -> Poly {
    match self.distill_expr_poly(stage, inner) {
      Poly::Error(err) => Poly::Error(err),
      Poly::Value(p) => Poly::Space(p),
      Poly::Place((p, q)) => Poly::Place((q, p)),
      Poly::Space(p) => Poly::Value(p),
    }
  }

  pub(crate) fn distill_pat_value_inverse(&mut self, stage: &mut Stage, inner: &TirPat) -> Port {
    self.distill_pat_space(stage, inner)
  }

  pub(crate) fn distill_pat_space_inverse(&mut self, stage: &mut Stage, inner: &TirPat) -> Port {
    self.distill_pat_value(stage, inner)
  }

  pub(crate) fn distill_pat_place_inverse(
    &mut self,
    stage: &mut Stage,
    inner: &TirPat,
  ) -> (Port, Port) {
    let (value, space) = self.distill_pat_place(stage, inner);
    (space, value)
  }
}
