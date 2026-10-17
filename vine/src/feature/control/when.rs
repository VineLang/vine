use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprWhen {
  pub label: Label,
  pub arms: Children<WhenArm>,
}

impl ParseKind<Expr, ExprWhen> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct WhenArm {
  pub cond: Option<Expr>,
  pub body: Block,
}
