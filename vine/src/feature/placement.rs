use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Placement<T> {
  pub value: T,
  pub space: T,
}

impl ParsePratt<Expr, Placement<Expr>> for Parser<'_> {} // todo
impl ParsePratt<Pat, Placement<Pat>> for Parser<'_> {} // todo
