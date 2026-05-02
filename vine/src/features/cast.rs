use crate::{
  components::resolver::Resolver,
  structures::{
    ast::{Expr, Span, Ty},
    content::{Content, Indent, Keyword, Punct, Space},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
  },
  tools::fmt::{Chain, ChainKind, Formatter},
};

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_cast(&self, expr: &Expr, ty: &Ty, postfix: bool) -> Chain {
    if postfix {
      self.chain_expr(expr).chain(
        ChainKind::Postfix,
        Content::even((
          Punct("."),
          Keyword("as"),
          Punct("["),
          Indent::eager(self.fmt_ty(ty)),
          Punct("]"),
        )),
      )
    } else {
      self
        .chain_expr(expr)
        .chain(ChainKind::As, Content::even((Space, Keyword("as"), Space, self.fmt_ty(ty))))
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
    Ok(TirExpr::new(span, to_ty, TirExprKind::Call(rel, vec![inner])))
  }
}
