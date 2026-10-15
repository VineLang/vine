use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprIf {
  pub label: Label,
  pub cond: Expr,
  pub then: Block,
  pub else_: Option<Block>,
}
