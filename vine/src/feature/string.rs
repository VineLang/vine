use super::*;

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

impl ParsePratt<Expr, StringLiteral<Expr>> for Parser<'_> {} // ugh
impl ParsePratt<Pat, StringLiteral<Pat>> for Parser<'_> {} // ugh
