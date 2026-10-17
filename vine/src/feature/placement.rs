use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprPlacement {
  pub value: Expr,
  pub space: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatPlacement {
  pub value: Pat,
  pub space: Pat,
}

impl ParsePratt<Expr, ExprPlacement> for Parser<'_> {} // todo
impl ParsePratt<Pat, PatPlacement> for Parser<'_> {} // todo
