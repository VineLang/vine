use vine_util::parser::Parser;

use crate::{
  components::{distiller::Distiller, lexer::Token, parser::VineParser, resolver::Resolver},
  structures::{
    ast::{ExprKind, Span},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::Type,
    vir::{Port, PortKind},
  },
};

impl VineParser<'_> {
  pub(crate) fn parse_expr_numeric(&mut self) -> Result<ExprKind, Diag> {
    let span = self.span();
    let token = self.expect(Token::Num)?;
    if token.contains('.') {
      Ok(ExprKind::F32(self.parse_f32_like(token, |_| Diag::InvalidNum { span })?))
    } else {
      Ok(ExprKind::N32(self.parse_u32_like(token, |_| Diag::InvalidNum { span })?))
    }
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_n32(&mut self, span: Span, n: u32) -> Result<TirExpr, Diag> {
    let ty = self.builtin_ty(span, "N32", self.chart.builtins.n32);
    Ok(TirExpr::new(span, ty, TirExprKind::N32(n)))
  }

  pub(crate) fn resolve_expr_f32(&mut self, span: Span, n: f32) -> Result<TirExpr, Diag> {
    let ty = self.builtin_ty(span, "F32", self.chart.builtins.f32);
    Ok(TirExpr::new(span, ty, TirExprKind::F32(n)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_n32(&mut self, ty: Type, n: u32) -> Port {
    Port { ty, kind: PortKind::N32(n) }
  }

  pub(crate) fn distill_expr_value_f32(&mut self, ty: Type, f: f32) -> Port {
    Port { ty, kind: PortKind::F32(f) }
  }
}
