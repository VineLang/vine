use vine_util::parser::Parser;

use crate::{
  components::{distiller::Distiller, lexer::Token, parser::VineParser, resolver::Resolver},
  structures::{
    ast::{Block, Expr, ExprKind, Span},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::Type,
    vir::{Port, Stage},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_expr_if(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    self.expect(Token::If)?;
    let cond = self.parse_expr()?;
    let then = self.parse_block()?;
    let else_ = self.eat(Token::Else)?.then(|| self.parse_block()).transpose()?;
    Ok(ExprKind::If(cond, then, else_))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_if(
    &self,
    cond: &Expr<'core>,
    then: &Block<'core>,
    else_: &Option<Block<'core>>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("if "),
      self.fmt_expr(cond),
      Doc(" "),
      self.fmt_block(then, true),
      match else_ {
        Some(else_) => Doc::concat([Doc(" else "), self.fmt_block(else_, true)]),
        None => Doc(""),
      },
    ])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_if(
    &mut self,
    span: Span,
    cond: &Expr<'core>,
    then: &Block<'core>,
    else_: &Option<Block<'core>>,
  ) -> Result<TirExpr, Diag<'core>> {
    let result = if else_.is_some() { self.types.new_var(span) } else { self.types.nil() };
    self.enter_scope();
    let cond = self.resolve_scoped_cond(cond);
    let then = self.resolve_block_type(then, result);
    self.exit_scope();
    let else_ = else_.as_ref().map(|leg| self.resolve_block_type(leg, result));
    Ok(TirExpr::new(span, result, TirExprKind::If(cond, then, else_)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_if(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    cond: &TirExpr,
    then: &TirExpr,
    else_: &Option<TirExpr>,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (mut layer, mut init_stage) = self.child_layer(stage, span);

    let (mut then_stage, mut else_stage) =
      self.distill_cond(&mut layer, &mut init_stage, span, cond);
    let result = self.distill_expr_value(&mut then_stage, then);
    then_stage.local_barrier_write_to(local, result);
    self.finish_stage(init_stage);
    self.finish_stage(then_stage);

    if let Some(leg) = else_ {
      let result = self.distill_expr_value(&mut else_stage, leg);
      else_stage.local_barrier_write_to(local, result);
    }

    self.finish_stage(else_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }
}
