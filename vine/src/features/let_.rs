use vine_util::parser::Parse;

use crate::{
  components::{distiller::Distiller, lexer::Token, parser::Parser, resolver::Resolver},
  structures::{
    ast::{LetStmt, LetStmtKind, Span, Stmt, StmtKind},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirExprLetKind, TirPat},
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
    let loop_ = self.eat(Token::Loop)?;
    let bind = self.parse_pat()?;
    let kind = if loop_ {
      LetStmtKind::Loop
    } else if self.eat(Token::Eq)? {
      LetStmtKind::Init(self.parse_expr()?)
    } else {
      LetStmtKind::Uninit
    };
    self.eat(Token::Semi)?;
    Ok(StmtKind::Let(LetStmt { bind, kind }))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_stmt_let(&self, stmt: &LetStmt) -> Doc<'src> {
    Doc::concat([
      Doc("let "),
      match &stmt.kind {
        LetStmtKind::Init(expr) => {
          Doc::concat([self.fmt_pat(&stmt.bind), Doc(" = "), self.fmt_expr(expr)])
        }
        LetStmtKind::Uninit => self.fmt_pat(&stmt.bind),
        LetStmtKind::Loop => Doc::concat([Doc("loop "), self.fmt_pat(&stmt.bind)]),
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
    let kind = match &stmt.kind {
      LetStmtKind::Init(expr) => TirExprLetKind::Init(self.resolve_expr(expr)),
      LetStmtKind::Uninit => TirExprLetKind::Uninit,
      LetStmtKind::Loop => TirExprLetKind::Loop,
    };
    let bind = self.resolve_pat(&stmt.bind);
    if let TirExprLetKind::Init(init) = &kind {
      self.expect_type(init.span, init.ty, bind.ty);
    }
    TirExprKind::Let(bind, kind, self.resolve_stmts_type(span, rest, ty))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_let(
    &mut self,
    stage: &mut Stage,
    pat: &TirPat,
    kind: &TirExprLetKind,
    continuation: &TirExpr,
  ) -> Port {
    match kind {
      TirExprLetKind::Init(init) => {
        let value = self.distill_expr_value(stage, init);
        let pat = self.distill_pat_value(stage, pat);
        stage.steps.push(Step::Link(pat, value));
      }
      TirExprLetKind::Uninit => {
        self.distill_pat_uninit(stage, pat);
      }
      TirExprLetKind::Loop => {
        self.distill_pat_loop(stage, pat);
      }
    }
    self.distill_expr_value(stage, continuation)
  }
}
