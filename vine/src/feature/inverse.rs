use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprInverse {
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatInverse {
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyInverse {
  pub child: Ty,
}
