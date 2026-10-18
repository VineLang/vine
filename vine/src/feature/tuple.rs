use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Tuple<T> {
  pub children: Sequence<T>,
}

impl ParsePratt<Expr, Tuple<Expr>> for Parser<'_> {} // todo
impl ParsePratt<Pat, Tuple<Pat>> for Parser<'_> {} // todo
impl ParsePratt<Ty, Tuple<Ty>> for Parser<'_> {} // todo
