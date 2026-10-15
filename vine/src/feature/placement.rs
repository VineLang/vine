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
