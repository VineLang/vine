use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprList {
  pub children: Sequence<Expr>,
}

impl ParsePratt<Expr, ExprList> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatList {
  pub children: Sequence<Pat>,
}

impl ParsePratt<Pat, PatList> for Parser<'_> {} // todo
