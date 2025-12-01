use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::Distiller,
    lexer::Token,
    parser::{BRACE, VineParser},
    resolver::Resolver,
  },
  structures::{
    ast::{LetElse, LetStmt, Span, Stmt, StmtKind},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::Type,
    vir::{Port, Stage, Step},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl VineParser<'_> {
  pub(crate) fn parse_stmt_let(&mut self) -> Result<StmtKind, Diag> {
    self.expect(Token::Let)?;
    if self.check(Token::Fn) {
      return self._parse_stmt_let_fn();
    }
    let bind = self.parse_pat()?;
    let init = self.eat_then(Token::Eq, Self::parse_expr)?;
    let else_ = if init.is_some() && self.eat(Token::Else)? {
      Some(if self.eat(Token::Match)? {
        let arms =
          self.parse_delimited(BRACE, |self_| Ok((self_.parse_pat()?, self_.parse_block()?)))?;
        LetElse::Match(arms)
      } else {
        LetElse::Block(self.parse_block()?)
      })
    } else {
      None
    };
    self.eat(Token::Semi)?;
    Ok(StmtKind::Let(LetStmt { bind, init, else_ }))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_stmt_let(&self, stmt: &LetStmt) -> Doc<'src> {
    if let Some(LetElse::Block(else_)) = &stmt.else_ {
      Doc::concat([
        Doc("assert "),
        match &stmt.init {
          Some(e) => Doc::concat([self.fmt_expr(e), Doc(" is ")]),
          None => Doc::EMPTY,
        },
        self.fmt_pat(&stmt.bind),
        Doc(" else "),
        self.fmt_block(else_, false),
      ])
    } else {
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
    match &stmt.else_ {
      None => {
        let bind = self.resolve_pat(&stmt.bind);
        if let Some(init) = &init {
          self.expect_type(init.span, init.ty, bind.ty);
        }
        TirExprKind::Let(bind, init, self.resolve_stmts_type(span, rest, ty))
      }
      Some(else_) => {
        let init = init.unwrap();
        self.enter_scope();
        let mut bind = self.resolve_pat(&stmt.bind);
        if let Some(err) = self.expect_type(init.span, init.ty, bind.ty) {
          bind = self.error_pat(bind.span, err.into());
        }
        let rest = self.resolve_stmts_type(span, rest, ty);
        self.exit_scope();
        let mut arms = vec![(bind, rest)];
        match else_ {
          LetElse::Block(block) => {
            let block = self.resolve_block_type(block, ty);
            arms.push((TirPat::new(span, init.ty, TirPatKind::Hole), block));
          }
          LetElse::Match(arms_) => {
            for (pat, block) in arms_ {
              self.enter_scope();
              let pat = self.resolve_pat_type(pat, init.ty);
              let block = self.resolve_block_type(block, ty);
              self.exit_scope();
              arms.push((pat, block));
            }
          }
        }
        TirExprKind::Match(init, arms)
      }
    }
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
