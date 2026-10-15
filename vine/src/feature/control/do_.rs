use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprDo {
  pub label: Label,
  pub body: Block,
}
