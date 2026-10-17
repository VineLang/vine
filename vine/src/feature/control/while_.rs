use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprWhile {
  pub label: Label,
  pub cond: Expr,
  pub body: Block,
  pub else_: Option<Block>,
}

impl ParseKind<Expr, ExprWhile> for Parser<'_> {} // todo
