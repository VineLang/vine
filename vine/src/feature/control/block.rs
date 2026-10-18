use crate::feature::{
  control::{
    closure::{ExprClosure, StmtClosure},
    let_::{LetKind, StmtLet},
  },
  operator::{AssignOp, StmtAssign},
};

use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Block {
  pub span: Span,
  pub pat: Option<Pat>,
  pub open_brace: Spanned<()>,
  pub stmts: Children<Stmt>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct StmtExpr {
  pub expr: Expr,
  /// Whether this expression statement is terminated by a semicolon.
  pub semi: bool,
}

impl Parse<Block> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Block>, Diag> {
    if self.check(Token::Eq) || self.check(Token::OpenBrace) {
      let span = self.start_span();
      let pat = self.eat_then(Token::Eq, Self::parse)?;
      let open_brace = self.token_span();
      let stmts = self.parse_children(Some(Token::OpenBrace), None, Some(Token::CloseBrace))?;
      let span = self.end_span(span);
      Ok(Some(Block { span, pat, open_brace, stmts }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<Stmt> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Stmt>, Diag> {
    let span = self.start_span();
    if let Some(kind) = self.try_parse()? {
      let span = self.end_span(span);
      Ok(Some(Stmt { span, kind: Box::new(kind) }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<StmtKind> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtKind>, Diag> {
    for_enum!(Kind in StmtKind { $(
      if let Some(kind) = self.try_parse()? {
        return Ok(Some(StmtKind::$Kind(kind)))
      }
    )* });

    if self.eat(Token::Let)? {
      if self.eat(Token::Fn)? {
        let flex = self.parse()?;
        let name = self.parse()?;
        let args = self.parse()?;
        let return_ty = self.eat_then(Token::Colon, Self::parse)?;
        let body = self.parse()?;
        return Ok(Some(StmtKind::Closure(StmtClosure {
          name,
          closure: ExprClosure { flex, args, return_ty, body },
        })));
      } else {
        let loop_ = self.eat(Token::Loop)?;
        let pat = self.parse()?;
        let kind = if loop_ {
          LetKind::Loop
        } else if self.eat(Token::Eq)? {
          LetKind::Init(self.parse()?)
        } else {
          LetKind::Uninit
        };
        self.semi()?;
        return Ok(Some(StmtKind::Let(StmtLet { pat, kind })));
      }
    }

    if let Some(expr) = self._parse_full()? {
      return Ok(Some(StmtKind::Expr(StmtExpr { expr, semi: false })));
    }

    if let Some(expr) = self.try_parse_pratt(Prec::Min)? {
      if let Some(op) = self.try_parse::<Spanned<AssignOp>>()? {
        let value = self.parse()?;
        self.semi()?;
        return Ok(Some(StmtKind::Assign(StmtAssign { assignee: expr, op, value })));
      }
      let semi = self.check(Token::Semi) || !self.check(Token::CloseBrace);
      if semi {
        self.semi()?;
      }
      return Ok(Some(StmtKind::Expr(StmtExpr { expr, semi })));
    }

    Ok(None)
  }
}

// Handled by `Parse<Stmtind>`
impl Parse<StmtExpr> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtExpr>, Diag> {
    Ok(None)
  }
}
