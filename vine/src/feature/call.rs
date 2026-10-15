use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprCall {
  pub callee: Expr,
  pub args: Sequence<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TraitFn {
  pub callee: Ty,
  pub args: Sequence<Ty>,
  pub return_: Option<Ty>,
}
