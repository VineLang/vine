use crate::component::lexer::Token;

use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtAssign {
  pub assignee: Expr,
  pub op: Spanned<AssignOp>,
  pub value: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprBinaryOp {
  pub lhs: Expr,
  pub op: Spanned<BinaryOp>,
  pub rhs: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprUnaryOp {
  pub op: Spanned<UnaryOp>,
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprComparisonOp {
  pub head: Expr,
  pub comparisons: Vec<Comparison>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatComparisonOp {
  pub pat: Option<Pat>,
  pub comparison: Comparison,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Comparison {
  pub op: Spanned<ComparisonOp>,
  pub value: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprCast {
  pub value: Expr,
  pub op: Spanned<()>,
  pub ty: Ty,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprIndex {
  pub container: Expr,
  pub index: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ExprRange {
  Exclusive(Option<Expr>, Option<Expr>),
  Inclusive(Option<Expr>, Expr),
}

#[derive_(Debug, Clone, Copy, Visit!)]
pub enum AssignOp {
  Normal,
  Inverse,
  BinaryOp(BinaryOp),
}

#[derive_(Debug, Clone, Copy, Visit!, for_enum!)]
pub enum BinaryOp {
  BitOr,
  BitXor,
  BitAnd,
  Shl,
  Shr,
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Concat,
  Pow,
}

#[derive_(Debug, Clone, Copy, Visit!, for_enum!)]
pub enum UnaryOp {
  Pos,
  Neg,
}

#[derive_(Debug, Clone, Copy, Visit!, for_enum!)]
pub enum ComparisonOp {
  Eq,
  Ne,
  Lt,
  Gt,
  Le,
  Ge,
  In,
}

impl AssignOp {
  pub fn token(self) -> Token {
    match self {
      AssignOp::Normal => Token::Eq,
      AssignOp::Inverse => Token::TildeEq,
      AssignOp::BinaryOp(op) => op.token_eq(),
    }
  }
}

impl BinaryOp {
  pub fn precedence(self) -> Prec {
    match self {
      BinaryOp::BitOr => Prec::BitOr,
      BinaryOp::BitXor => Prec::BitXor,
      BinaryOp::BitAnd => Prec::BitAnd,
      BinaryOp::Shl => Prec::BitShift,
      BinaryOp::Shr => Prec::BitShift,
      BinaryOp::Add => Prec::Additive,
      BinaryOp::Sub => Prec::Additive,
      BinaryOp::Mul => Prec::Multiplicative,
      BinaryOp::Div => Prec::Multiplicative,
      BinaryOp::Rem => Prec::Multiplicative,
      BinaryOp::Concat => Prec::Additive,
      BinaryOp::Pow => Prec::Exponential,
    }
  }

  fn tokens(self) -> (Token, Token) {
    match self {
      BinaryOp::BitOr => (Token::Pipe, Token::PipeEq),
      BinaryOp::BitXor => (Token::Caret, Token::CaretEq),
      BinaryOp::BitAnd => (Token::Amp, Token::AmpEq),
      BinaryOp::Shl => (Token::Shl, Token::ShlEq),
      BinaryOp::Shr => (Token::Shr, Token::ShrEq),
      BinaryOp::Add => (Token::Plus, Token::PlusEq),
      BinaryOp::Sub => (Token::Minus, Token::MinusEq),
      BinaryOp::Mul => (Token::Star, Token::StarEq),
      BinaryOp::Div => (Token::Slash, Token::SlashEq),
      BinaryOp::Rem => (Token::Percent, Token::PercentEq),
      BinaryOp::Concat => (Token::PlusPlus, Token::PlusPlusEq),
      BinaryOp::Pow => (Token::StarStar, Token::StarStarEq),
    }
  }

  pub fn token(self) -> Token {
    self.tokens().0
  }

  pub fn token_eq(self) -> Token {
    self.tokens().1
  }
}

impl UnaryOp {
  pub fn token(self) -> Token {
    match self {
      UnaryOp::Pos => Token::Plus,
      UnaryOp::Neg => Token::Minus,
    }
  }
}

impl ComparisonOp {
  pub fn token(self) -> Token {
    match self {
      ComparisonOp::Eq => Token::EqEq,
      ComparisonOp::Ne => Token::BangEq,
      ComparisonOp::Lt => Token::Lt,
      ComparisonOp::Gt => Token::Gt,
      ComparisonOp::Le => Token::LtEq,
      ComparisonOp::Ge => Token::GtEq,
      ComparisonOp::In => Token::In,
    }
  }
}

// Handled directly by `Parse<StmtKind>`
impl Parse<StmtAssign> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtAssign>, Diag> {
    Ok(None)
  }
}

impl Parse<AssignOp> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<AssignOp>, Diag> {
    for_enum!(Op in BinaryOp {
      if self.eat(Token::Eq)? {
        Ok(Some(AssignOp::Normal))
      } else if self.eat(Token::TildeEq)? {
        Ok(Some(AssignOp::Inverse))
      } $(else if self.eat(BinaryOp::$Op.token_eq())? {
        Ok(Some(AssignOp::BinaryOp(BinaryOp::$Op)))
      })* else {
        Ok(None)
      }
    })
  }
}

impl ParsePratt<Expr, ExprBinaryOp> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: Expr,
  ) -> Result<Result<ExprBinaryOp, Expr>, Diag> {
    let span = self.token_span().span;
    for_enum!(Op in BinaryOp { $(
      if let Some(prec) = ctx.prec(BinaryOp::$Op.precedence()) && self.eat(BinaryOp::$Op.token())? {
        let op = Spanned { span, inner: BinaryOp::$Op };
        let rhs = self.parse_pratt(BinaryOp::$Op.precedence())?;
        return Ok(Ok(ExprBinaryOp { lhs: initial, op, rhs } ));
      }
    )* });
    Ok(Err(initial))
  }
}

impl ParsePratt<Expr, ExprUnaryOp> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<ExprUnaryOp>, Diag> {
    let span = self.token_span().span;
    for_enum!(Op in UnaryOp { $(
      if self.eat(UnaryOp::$Op.token())? {
        let op = Spanned { span, inner: UnaryOp::$Op };
        let child = self.parse_pratt(Prec::Prefix)?;
        return Ok(Some(ExprUnaryOp { op, child }));
      }
    )* });
    Ok(None)
  }
}

impl ParsePratt<Expr, ExprComparisonOp> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: Expr,
  ) -> Result<Result<ExprComparisonOp, Expr>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Comparison)
      && let Some(comparison) = self.try_parse::<Comparison>()?
    {
      _ = prec();
      let mut comparisons = vec![comparison];
      while let Some(comparison) = self.try_parse::<Comparison>()? {
        comparisons.push(comparison);
      }
      Ok(Ok(ExprComparisonOp { head: initial, comparisons }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl ParsePratt<Pat, PatComparisonOp> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<PatComparisonOp>, Diag> {
    if let Some(comparison) = self.try_parse::<Comparison>()? {
      Ok(Some(PatComparisonOp { pat: None, comparison }))
    } else {
      Ok(None)
    }
  }

  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: Pat,
  ) -> Result<Result<PatComparisonOp, Pat>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Comparison)
      && let Some(comparison) = self.try_parse::<Comparison>()?
    {
      _ = prec();
      Ok(Ok(PatComparisonOp { pat: Some(initial), comparison }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl Parse<Comparison> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Comparison>, Diag> {
    if let Some(op) = self.try_parse::<Spanned<ComparisonOp>>()? {
      let value = self.parse_pratt::<Expr>(Prec::Comparison)?;
      Ok(Some(Comparison { op, value }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<ComparisonOp> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ComparisonOp>, Diag> {
    for_enum!(Op in ComparisonOp {
      $(if self.eat(ComparisonOp::$Op.token())? {
        Ok(Some(ComparisonOp::$Op))
      } else)* {
        Ok(None)
      }
    })
  }
}

impl ParsePratt<Expr, ExprCast> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: Expr,
  ) -> Result<Result<ExprCast, Expr>, Diag> {
    let op = self.token_span();
    if let Some(prec) = ctx.prec(Prec::Annotation)
      && self.eat(Token::As)?
    {
      let ty = self.parse_pratt(prec())?;
      Ok(Ok(ExprCast { value: initial, op, ty }))
    } else {
      Ok(Err(initial))
    }
  }

  fn _parse_dot(&mut self, initial: Expr) -> Result<Result<ExprCast, Expr>, Diag> {
    let op = self.token_span();
    if self.eat(Token::As)? {
      self.expect(Token::OpenBracket)?;
      let ty = self.parse()?;
      self.expect(Token::OpenBracket)?;
      Ok(Ok(ExprCast { value: initial, op, ty }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl ParsePratt<Expr, ExprIndex> for Parser<'_> {
  fn _parse_dot(&mut self, initial: Expr) -> Result<Result<ExprIndex, Expr>, Diag> {
    if self.eat(Token::OpenBracket)? {
      let index = self.parse()?;
      self.expect(Token::CloseBracket)?;
      Ok(Ok(ExprIndex { container: initial, index }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl ParsePratt<Expr, ExprRange> for Parser<'_> {
  fn _parse_initial(&mut self, ctx: &mut PrecCtx) -> Result<Option<ExprRange>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Range) {
      if self.eat(Token::DotDot)? {
        Ok(Some(ExprRange::Exclusive(None, self.try_parse_pratt(prec())?)))
      } else if self.eat(Token::DotDotEq)? {
        Ok(Some(ExprRange::Inclusive(None, self.parse_pratt(prec())?)))
      } else {
        Ok(None)
      }
    } else {
      Ok(None)
    }
  }

  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: Expr,
  ) -> Result<Result<ExprRange, Expr>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Range) {
      if self.eat(Token::DotDot)? {
        Ok(Ok(ExprRange::Exclusive(Some(initial), self.try_parse_pratt(prec())?)))
      } else if self.eat(Token::DotDotEq)? {
        Ok(Ok(ExprRange::Inclusive(Some(initial), self.parse_pratt(prec())?)))
      } else {
        Ok(Err(initial))
      }
    } else {
      Ok(Err(initial))
    }
  }
}
