use crate::{
  components::resolver::Resolver,
  structures::{
    ast::{Expr, Span, Ty},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_cast(&self, expr: &Expr, ty: &Ty, postfix: bool) -> Doc<'src> {
    if postfix {
      Doc::concat([self.fmt_expr(expr), Doc(".as["), self.fmt_ty(ty), Doc("]")])
    } else {
      Doc::concat([self.fmt_expr(expr), Doc(" as "), self.fmt_ty(ty)])
    }
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_cast(
    &mut self,
    span: Span,
    inner: &Expr,
    to: &Ty,
  ) -> Result<TirExpr, Diag> {
    let inner = self.resolve_expr(inner);
    let to_ty = self.resolve_ty(to, true);
    let rel = self.builtin_fn(span, self.chart.builtins.cast, "cast", [inner.ty, to_ty])?;
    Ok(TirExpr::new(span, to_ty, TirExprKind::Call(rel, None, vec![inner])))
  }
}
