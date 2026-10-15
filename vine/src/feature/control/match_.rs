use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprMatch {
  pub label: Label,
  pub value: Expr,
  pub arms: Children<MatchArm>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct MatchArm {
  pub pat: Pat,
  pub body: Block,
}
