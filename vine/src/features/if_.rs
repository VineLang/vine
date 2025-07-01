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
    let mut arms = Vec::new();
    loop {
      let cond = self.parse_expr()?;
      let then = self.parse_block()?;
      arms.push((cond, then));
      if self.eat(Token::Else)? {
        if self.eat(Token::If)? {
          continue;
        } else {
          let leg = self.parse_block()?;
          return Ok(ExprKind::If(arms, Some(leg)));
        }
      } else {
        return Ok(ExprKind::If(arms, None));
      }
    }
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_if(
    &self,
    arms: &Vec<(Expr<'core>, Block<'core>)>,
    leg: &Option<Block<'core>>,
  ) -> Doc<'src> {
    Doc::interleave(
      arms
        .iter()
        .map(|(cond, block)| {
          Doc::concat([Doc("if "), self.fmt_expr(cond), Doc(" "), self.fmt_block(block, true)])
        })
        .chain(leg.iter().map(|block| self.fmt_block(block, true))),
      Doc(" else "),
    )
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_if(
    &mut self,
    span: Span,
    arms: &Vec<(Expr<'core>, Block<'core>)>,
    leg: &Option<Block<'core>>,
  ) -> Result<TirExpr, Diag<'core>> {
    let result = if leg.is_some() { self.types.new_var(span) } else { self.types.nil() };
    let arms = Vec::from_iter(arms.iter().map(|(cond, block)| {
      self.enter_scope();
      let cond = self.resolve_scoped_cond(cond);
      let block = self.resolve_block_type(block, result);
      self.exit_scope();
      (cond, block)
    }));
    let leg = leg.as_ref().map(|leg| self.resolve_block_type(leg, result));
    Ok(TirExpr::new(span, result, TirExprKind::If(arms, leg)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_if(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    arms: &[(TirExpr, TirExpr)],
    leg: &Option<TirExpr>,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (mut layer, mut cur_stage) = self.child_layer(stage, span);

    for (cond, block) in arms {
      let (mut then_stage, else_stage) = self.distill_cond(&mut layer, &mut cur_stage, span, cond);
      let result = self.distill_expr_value(&mut then_stage, block);
      then_stage.local_barrier_write_to(local, result);
      self.finish_stage(cur_stage);
      self.finish_stage(then_stage);
      cur_stage = else_stage;
    }

    if let Some(leg) = leg {
      let result = self.distill_expr_value(&mut cur_stage, leg);
      cur_stage.local_barrier_write_to(local, result);
    }

    self.finish_stage(cur_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }
}
