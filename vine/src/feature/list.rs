use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprList {
  pub children: Sequence<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatList {
  pub children: Sequence<Pat>,
}
