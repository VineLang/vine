use vine_util::parser::Parser;

use crate::{
  components::{lexer::Token, parser::VineParser, resolver::Resolver},
  structures::{
    ast::{AssertStmt, Span, Stmt, StmtKind},
    diag::Diag,
    tir::TirExprKind,
    types::Type,
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl VineParser<'_> {
  pub(crate) fn parse_stmt_assert(&mut self) -> Result<StmtKind, Diag> {
    self.expect(Token::Assert)?;
    let expr = self.parse_expr()?;
    let else_ = self.eat_then(Token::Else, Self::parse_block)?;
    Ok(StmtKind::Assert(AssertStmt { expr, else_ }))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_stmt_assert(&self, stmt: &AssertStmt) -> Doc<'src> {
    Doc::concat([
      Doc("assert "),
      self.fmt_expr(&stmt.expr),
      match &stmt.else_ {
        Some(b) => Doc::concat([Doc(" else "), self.fmt_block(b, false)]),
        None => Doc::EMPTY,
      },
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_stmts_assert(
    &mut self,
    span: Span,
    ty: Type,
    stmt: &AssertStmt,
    rest: &[Stmt],
  ) -> TirExprKind {
    self.enter_scope();
    let expr = self.resolve_scoped_cond(&stmt.expr);
    let rest = self.resolve_stmts_type(span, rest, ty);
    self.exit_scope();
    let else_ = stmt.else_.as_ref().map(|else_| self.resolve_block_type(else_, ty));
    TirExprKind::If(expr, rest, else_)
  }
}
