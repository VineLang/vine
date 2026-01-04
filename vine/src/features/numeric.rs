use ivy::ast::Tree;
use vine_util::{nat::Nat, parser::Parse};

use crate::{
  components::{
    distiller::Distiller, emitter::Emitter, lexer::Token, parser::Parser, resolver::Resolver,
  },
  structures::{
    ast::{ExprKind, Span, Ty},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::{Inverted, Type, TypeKind},
    vir::{Port, PortKind},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_expr_numeric(&mut self) -> Result<ExprKind, Diag> {
    let span = self.span();
    let token = self.expect(Token::Num)?;
    let ty = self.eat_then(Token::OpenBracket, |self_| {
      let ty = self_.parse_ty()?;
      self_.expect(Token::CloseBracket)?;
      Ok(ty)
    })?;
    match (token.contains('.'), ty) {
      (true, Some(ty)) => {
        Ok(ExprKind::Float(span, self.parse_f64_like(token, |_| Diag::InvalidNum { span })?, ty))
      }
      (true, None) => Ok(ExprKind::F32(self.parse_f32_like(token, |_| Diag::InvalidNum { span })?)),
      (false, Some(ty)) => {
        Ok(ExprKind::Nat(span, self.parse_nat_like(token, |_| Diag::InvalidNum { span })?, ty))
      }
      (false, None) => {
        Ok(ExprKind::N32(self.parse_u32_like(token, |_| Diag::InvalidNum { span })?))
      }
    }
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_nat(&self, lit_span: Span, ty: &Ty) -> Doc<'src> {
    Doc::concat([self.fmt_verbatim(lit_span), Doc("["), self.fmt_ty(ty), Doc("]")])
  }

  pub(crate) fn fmt_expr_float(&self, lit_span: Span, ty: &Ty) -> Doc<'src> {
    Doc::concat([self.fmt_verbatim(lit_span), Doc("["), self.fmt_ty(ty), Doc("]")])
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

  pub(crate) fn resolve_expr_float(
    &mut self,
    span: Span,
    n: f64,
    ty: &Ty,
  ) -> Result<TirExpr, Diag> {
    let ty = self.resolve_ty(ty, true);
    match self.types.kind(ty) {
      Some((Inverted(false), TypeKind::Opaque(type_id, _)))
        if Some(*type_id) == self.chart.builtins.f64 =>
      {
        Ok(TirExpr::new(span, ty, TirExprKind::F64(n)))
      }
      Some((Inverted(false), TypeKind::Opaque(type_id, _)))
        if Some(*type_id) == self.chart.builtins.f32 =>
      {
        Ok(TirExpr::new(span, ty, TirExprKind::F32(n as f32)))
      }
      _ => Err(self.diags.error(Diag::InvalidNum { span }))?,
    }
  }

  pub(crate) fn resolve_expr_nat(&mut self, span: Span, n: &Nat, ty: &Ty) -> Result<TirExpr, Diag> {
    let Some(nat_ty) = self.chart.builtins.nat else {
      Err(self.diags.error(Diag::MissingBuiltin { span, builtin: "Nat" }))?
    };
    let nat_ty = self.types.new(TypeKind::Struct(nat_ty, vec![]));
    let ty = self.resolve_ty(ty, true);
    let fn_rel = self.builtin_fn(span, self.chart.builtins.cast, "cast", [nat_ty, ty])?;
    Ok(TirExpr::new(
      span,
      ty,
      TirExprKind::Call(
        fn_rel,
        None,
        vec![TirExpr::new(span, nat_ty, TirExprKind::Nat(n.clone()))],
      ),
    ))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_n32(&mut self, ty: Type, n: u32) -> Port {
    Port { ty, kind: PortKind::N32(n) }
  }

  pub(crate) fn distill_expr_value_f32(&mut self, ty: Type, f: f32) -> Port {
    Port { ty, kind: PortKind::F32(f) }
  }

  pub(crate) fn distill_expr_value_f64(&mut self, ty: Type, f: f64) -> Port {
    Port { ty, kind: PortKind::F64(f) }
  }

  pub(crate) fn distill_expr_value_nat(&mut self, ty: Type, n: &Nat) -> Port {
    Port { ty, kind: PortKind::Nat(n.clone()) }
  }
}

impl Emitter<'_> {
  pub(crate) fn emit_nat(&mut self, n: &Nat) -> Tree {
    self.build_list(&n.0, |_, &n| Tree::N32(n))
  }
}
