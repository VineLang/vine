use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprTuple {
  pub children: Sequence<Expr>,
}

impl ParsePratt<Expr, ExprTuple> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatTuple {
  pub children: Sequence<Pat>,
}

impl ParsePratt<Pat, PatTuple> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TyTuple {
  pub children: Sequence<Ty>,
}

impl ParsePratt<Ty, TyTuple> for Parser<'_> {} // todo
