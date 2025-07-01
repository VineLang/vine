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

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_expr_numeric(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    let span = self.span();
    let token = self.expect(Token::Num)?;
    if token.contains('.') {
      Ok(ExprKind::F32(self.parse_f32_like(token, |_| Diag::InvalidNum { span })?))
    } else if token.starts_with("+") || token.starts_with("-") {
      let abs = self.parse_u32_like(&token[1..], |_| Diag::InvalidNum { span })? as i32;
      let num = if token.starts_with("-") { -abs } else { abs };
      Ok(ExprKind::I32(num))
    } else {
      Ok(ExprKind::N32(self.parse_u32_like(token, |_| Diag::InvalidNum { span })?))
    }
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_n32(&mut self, span: Span, n: u32) -> Result<TirExpr, Diag<'core>> {
    let ty = self.builtin_ty(span, "N32", self.chart.builtins.n32);
    Ok(TirExpr::new(span, ty, TirExprKind::N32(n)))
  }

  pub(crate) fn resolve_expr_i32(&mut self, span: Span, n: i32) -> Result<TirExpr, Diag<'core>> {
    let ty = self.builtin_ty(span, "I32", self.chart.builtins.i32);
    Ok(TirExpr::new(span, ty, TirExprKind::I32(n)))
  }

  pub(crate) fn resolve_expr_f32(&mut self, span: Span, n: f32) -> Result<TirExpr, Diag<'core>> {
    let ty = self.builtin_ty(span, "F32", self.chart.builtins.f32);
    Ok(TirExpr::new(span, ty, TirExprKind::F32(n)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_expr_value_n32(&mut self, ty: Type, n: u32) -> Port {
    Port { ty, kind: PortKind::N32(n) }
  }

  pub(crate) fn distill_expr_value_i32(&mut self, ty: Type, i: i32) -> Port {
    Port { ty, kind: PortKind::N32(i as u32) }
  }

  pub(crate) fn distill_expr_value_f32(&mut self, ty: Type, f: f32) -> Port {
    Port { ty, kind: PortKind::F32(f) }
  }
}
