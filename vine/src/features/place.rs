use crate::{
  components::{distiller::Distiller, resolver::Resolver},
  structures::{
    ast::{Expr, Span},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    vir::{Port, Stage},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_place(&self, v: &Expr<'core>, s: &Expr<'core>) -> Doc<'src> {
    Doc::concat([Doc("("), self.fmt_expr(v), Doc("; "), self.fmt_expr(s), Doc(")")])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_place(
    &mut self,
    span: Span,
    value: &Expr<'core>,
    space: &Expr<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let value = self.resolve_expr(value);
    let space = self.resolve_expr(space);
    let ty = if self.types.unify(value.ty, space.ty).is_failure() {
      self.types.error(self.core.report(Diag::MismatchedValueSpaceTypes {
        span,
        value: self.types.show(self.chart, value.ty),
        space: self.types.show(self.chart, space.ty),
      }))
    } else {
      value.ty
    };
    Ok(TirExpr::new(span, ty, TirExprKind::Place(value, space)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_expr_place_place(
    &mut self,
    stage: &mut Stage,
    value: &TirExpr,
    space: &TirExpr,
  ) -> (Port, Port) {
    (self.distill_expr_value(stage, value), self.distill_expr_space(stage, space))
  }
}
