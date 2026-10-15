use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprAnnotation {
  pub expr: Expr,
  pub ty: Ty,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatAnnotation {
  pub pat: Pat,
  pub ty: Ty,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ImplAnnotation {
  pub impl_: Impl,
  pub trait_: Trait,
}
