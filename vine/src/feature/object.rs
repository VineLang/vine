use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprObject {
  pub children: Object<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatObject {
  pub children: Object<Pat>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyObject {
  pub children: Object<Ty>,
}
