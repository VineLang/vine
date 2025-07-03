use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::Distiller, lexer::Token, matcher::Row, parser::VineParser, resolver::Resolver,
  },
  structures::{
    ast::{LetStmt, Span, Stmt, StmtKind},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirPat},
    types::Type,
    vir::{Port, Stage, Step},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_stmt_let(&mut self) -> Result<StmtKind<'core>, Diag<'core>> {
    self.expect(Token::Let)?;
    if self.check(Token::Fn) {
      return self._parse_stmt_let_fn();
    }
    let bind = self.parse_pat()?;
    let init = self.eat(Token::Eq)?.then(|| self.parse_expr()).transpose()?;
    let else_block = self.eat(Token::Else)?.then(|| self.parse_block()).transpose()?;
    self.eat(Token::Semi)?;
    Ok(StmtKind::Let(LetStmt { bind, init, else_block }))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_stmt_let(&self, stmt: &LetStmt<'core>) -> Doc<'src> {
    Doc::concat([
      Doc("let "),
      self.fmt_pat(&stmt.bind),
      match &stmt.init {
        Some(e) => Doc::concat([Doc(" = "), self.fmt_expr(e)]),
        None => Doc::EMPTY,
      },
      match &stmt.else_block {
        Some(b) => Doc::concat([Doc(" else "), self.fmt_block(b, false)]),
        None => Doc::EMPTY,
      },
      Doc(";"),
    ])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_stmts_let(
    &mut self,
    span: Span,
    ty: Type,
    stmt: &LetStmt<'core>,
    rest: &[Stmt<'core>],
  ) -> TirExprKind {
    let init = stmt.init.as_ref().map(|init| self.resolve_expr(init));
    let else_block = stmt.else_block.as_ref().map(|block| self.resolve_block_type(block, ty));
    let bind = self.resolve_pat(&stmt.bind);
    if let Some(init) = &init {
      self.expect_type(init.span, init.ty, bind.ty);
    }
    if let Some(else_block) = else_block {
      TirExprKind::LetElse(bind, init.unwrap(), else_block, self.resolve_stmts_type(span, rest, ty))
    } else {
      TirExprKind::Let(bind, init, self.resolve_stmts_type(span, rest, ty))
    }
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_let(
    &mut self,
    stage: &mut Stage,
    pat: &TirPat,
    init: &Option<TirExpr>,
    continuation: &TirExpr,
  ) -> Port {
    if let Some(init) = init {
      let value = self.distill_expr_value(stage, init);
      let pat = self.distill_pat_value(stage, pat);
      stage.steps.push(Step::Link(pat, value));
    } else {
      self.distill_pat_nil(stage, pat);
    }
    self.distill_expr_value(stage, continuation)
  }

  pub(crate) fn distill_let_else(
    &mut self,
    stage: &mut Stage,
    span: Span,
    pat: &TirPat,
    init: &TirExpr,
    else_block: &TirExpr,
    continuation: &TirExpr,
  ) -> Port {
    let local = self.new_local(stage, span, continuation.ty);
    stage.local_barrier(local);
    let (mut layer, mut init_stage) = self.child_layer(stage, span);
    let value = self.distill_expr_value(&mut init_stage, init);
    let mut then_stage = self.new_unconditional_stage(&mut layer, span);
    let mut else_stage = self.new_unconditional_stage(&mut layer, span);
    self.distill_pattern_match(
      span,
      &mut layer,
      &mut init_stage,
      value,
      vec![Row::new(Some(pat), then_stage.interface), Row::new(None, else_stage.interface)],
    );
    let result = self.distill_expr_value(&mut then_stage, continuation);
    then_stage.local_barrier_write_to(local, result);
    let result = self.distill_expr_value(&mut else_stage, else_block);
    else_stage.local_barrier_write_to(local, result);
    self.finish_stage(init_stage);
    self.finish_stage(then_stage);
    self.finish_stage(else_stage);
    self.finish_layer(layer);
    stage.local_read_barrier(local, span, continuation.ty)
  }
}
