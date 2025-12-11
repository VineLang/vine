use vine_util::parser::Parse;

use crate::{
  components::{distiller::Distiller, lexer::Token, parser::Parser, resolver::Resolver},
  structures::{
    ast::{LetStmt, Span, Stmt, StmtKind},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirPat},
    types::Type,
    vir::{Port, Stage, Step},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_stmt_let(&mut self) -> Result<StmtKind, Diag> {
    self.expect(Token::Let)?;
    if self.check(Token::Fn) {
      return self._parse_stmt_let_fn();
    }
    let bind = self.parse_pat()?;
    let init = self.eat_then(Token::Eq, Self::parse_expr)?;
    self.eat(Token::Semi)?;
    Ok(StmtKind::Let(LetStmt { bind, init }))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_stmt_let(&self, stmt: &LetStmt) -> Doc<'src> {
    Doc::concat([
      Doc("let "),
      self.fmt_pat(&stmt.bind),
      match &stmt.init {
        Some(e) => Doc::concat([Doc(" = "), self.fmt_expr(e)]),
        None => Doc::EMPTY,
      },
      Doc(";"),
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_stmts_let(
    &mut self,
    span: Span,
    ty: Type,
    stmt: &LetStmt,
    rest: &[Stmt],
  ) -> TirExprKind {
    let init = stmt.init.as_ref().map(|init| self.resolve_expr(init));
    let bind = self.resolve_pat(&stmt.bind);
    if let Some(init) = &init {
      self.expect_type(init.span, init.ty, bind.ty);
    }
    TirExprKind::Let(bind, init, self.resolve_stmts_type(span, rest, ty))
  }
}

impl Distiller<'_> {
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
}
