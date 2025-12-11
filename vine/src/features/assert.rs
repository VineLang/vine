use vine_util::parser::Parse;

use crate::{
  components::{lexer::Token, parser::Parser, resolver::Resolver},
  structures::{
    ast::{AssertStmt, Span, Stmt, StmtKind},
    diag::Diag,
    tir::TirExprKind,
    types::Type,
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_stmt_assert(&mut self) -> Result<StmtKind, Diag> {
    self.expect(Token::Assert)?;
    let expr = self.parse_expr()?;
    self.expect(Token::Else)?;
    let else_ = self.parse_block()?;
    Ok(StmtKind::Assert(AssertStmt { expr, else_ }))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_stmt_assert(&self, stmt: &AssertStmt) -> Doc<'src> {
    Doc::concat([
      Doc("assert "),
      self.fmt_expr(&stmt.expr),
      Doc(" else "),
      self.fmt_block(&stmt.else_, false),
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
    let else_ = self.resolve_block_type(&stmt.else_, ty);
    TirExprKind::If(expr, rest, Some(else_))
  }
}
