use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprTry {
  pub result: Expr,
  pub try_: Spanned<()>,
}
