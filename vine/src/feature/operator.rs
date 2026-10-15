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
pub struct Comparison {
  pub op: Spanned<ComparisonOp>,
  pub value: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatComparisonOp {
  pub pat: Option<Pat>,
  pub comparison: Comparison,
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
pub struct ExprRange {
  pub kind: RangeKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum RangeKind {
  Inclusive(Option<Expr>, Option<Expr>),
  Exclusive(Option<Expr>, Expr),
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
