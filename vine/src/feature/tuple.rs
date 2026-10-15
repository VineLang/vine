use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprTuple {
  pub children: Sequence<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatTuple {
  pub children: Sequence<Pat>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyTuple {
  pub children: Sequence<Ty>,
}
