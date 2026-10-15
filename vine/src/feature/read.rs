use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprRead {
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatRead {
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyRead {
  pub child: Ty,
}
