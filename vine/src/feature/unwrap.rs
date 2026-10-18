use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Unwrap<T> {
  pub child: T,
  pub bang: Spanned<()>,
}

impl ParsePratt<Expr, Unwrap<Expr>> for Parser<'_> {} // todo
impl ParsePratt<Ty, Unwrap<Ty>> for Parser<'_> {} // todo
