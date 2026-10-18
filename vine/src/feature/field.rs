use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Field<T> {
  pub composite: T,
  pub field: FieldKind,
}

impl ParsePratt<Expr, Field<Expr>> for Parser<'_> {} // todo

impl ParsePratt<Ty, Field<Ty>> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub enum FieldKind {
  Tuple(Span, usize),
  Object(Spanned<Ident>),
}
