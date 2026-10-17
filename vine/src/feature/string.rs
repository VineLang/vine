use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprString {
  pub string: StringLiteral<Expr>,
}

impl ParsePratt<Expr, ExprString> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatString {
  pub string: StringLiteral<Pat>,
}

impl ParsePratt<Pat, PatString> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct StringLiteral<T = !> {
  pub span: Span,
  pub kind: StringKind,
  pub lang: Option<String>,
  pub lines: Vec<StringLine<T>>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum StringKind {
  Raw,
  Processed,
}

#[derive_(Debug, Clone, Visit!)]
pub struct StringLine<T = !> {
  pub pieces: Vec<StringPiece<T>>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum StringPiece<T = !> {
  Segment(String),
  Escape(char),
  Interpolation(T),
}
