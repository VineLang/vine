use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprMatch {
  pub label: Label,
  pub value: Expr,
  pub arms: Children<MatchArm>,
}

impl ParsePratt<Expr, ExprMatch> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct MatchArm {
  pub pat: Pat,
  pub body: Block,
}
