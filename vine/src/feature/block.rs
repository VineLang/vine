use super::*;

pub mod assert;
pub mod closure;
pub mod do_;
pub mod for_;
pub mod if_;
pub mod label;
pub mod let_;
pub mod loop_;
pub mod match_;
pub mod when;
pub mod while_;

use assert::StmtAssert;
use closure::{ExprClosure, StmtClosure};
use do_::ExprDo;
use for_::ExprFor;
use if_::ExprIf;
use label::{Label, StmtBreak, StmtContinue};
use let_::{LetKind, StmtLet};
use loop_::ExprLoop;
use match_::ExprMatch;
use operator::{AssignOp, StmtAssign};
use when::ExprWhen;
use while_::ExprWhile;

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum ExprBlock {
  Do(ExprDo),
  When(ExprWhen),
  For(ExprFor),
  Closure(ExprClosure),
  Loop(ExprLoop),
  While(ExprWhile),
  Match(ExprMatch),
  If(ExprIf),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Block {
  pub span: LocalSpan,
  pub pat: Option<Pat>,
  pub open_brace: Spanned<()>,
  pub stmts: Children<Stmt>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Stmt {
  pub span: LocalSpan,
  pub kind: Box<StmtKind>,
}

#[derive_(Debug, Clone, From, Visit!, for_enum!)]
pub enum StmtKind {
  Let(StmtLet),
  Closure(StmtClosure),
  Assert(StmtAssert),
  Assign(StmtAssign),
  Break(StmtBreak),
  Continue(StmtContinue),
  Expr(StmtExpr),
}

#[derive_(Debug, Clone, Visit!)]
pub struct StmtExpr {
  pub expr: Expr,
  /// Whether this expression statement is terminated by a semicolon.
  pub semi: bool,
}

impl GrammarPrattKind for ExprBlock {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Min {
      rules.push(grammar.hidden("Block", rule::<ExprBlock>()));
    }
  }
}

impl GrammarRule for ExprBlock {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ExprBlock",
      choice(for_enum!(Kind in ExprBlock { ($(
        rule_variant(ExprBlock::$Kind),
      )*) })),
    )
  }
}

impl GrammarRule for Block {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "Block",
      (
        (Token::Eq, rule::<Pat>().field("return")).optional(),
        Token::OpenBrace,
        rule::<Stmt>().repeat(false),
        below::<Expr>(Prec::Min).optional(),
        Token::CloseBrace,
      ),
    )
  }
}

impl GrammarRule for Stmt {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.hidden(
      "Stmt",
      choice(for_enum!(Kind in StmtKind { ($(
        rule_variant(StmtKind::$Kind),
      )*) })),
    )
  }
}

impl GrammarRule for StmtExpr {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar
      .named("StmtExpr", choice(((below::<Expr>(Prec::Block), Token::Semi), rule::<ExprBlock>())))
  }
}

impl ParsePratt<Expr, ExprBlock> for Parser<'_> {
  fn _parse_initial(&mut self, ctx: &mut PrecCtx) -> Result<Option<ExprBlock>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Min)
      && let Some(block) = self.try_parse::<ExprBlock>()?
    {
      _ = prec();
      Ok(Some(block))
    } else {
      Ok(None)
    }
  }
}

impl Parse<ExprBlock> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprBlock>, Diag> {
    for_enum!(Kind in ExprBlock { $(
      if let Some(kind) = self.try_parse()? {
        return Ok(Some(ExprBlock::$Kind(kind)));
      }
    )* });
    Ok(None)
  }
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
          closure: ExprClosure { flex, params: args, return_ty, body },
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
        self.semi(true)?;
        return Ok(Some(StmtKind::Let(StmtLet { pat, kind })));
      }
    }

    if let Some(Spanned { span, inner: block }) = self.try_parse::<Spanned<ExprBlock>>()? {
      let expr = Expr::new(span, block);
      return Ok(Some(StmtKind::Expr(StmtExpr { expr, semi: false })));
    }

    if let Some(expr) = self.try_parse_pratt(Prec::Block)? {
      if let Some(op) = self.try_parse::<Spanned<AssignOp>>()? {
        let value = self.parse()?;
        self.semi(true)?;
        return Ok(Some(StmtKind::Assign(StmtAssign { assignee: expr, op, value })));
      }
      let semi = self.check(Token::Semi) || !self.check(Token::CloseBrace);
      if semi {
        self.semi(true)?;
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
