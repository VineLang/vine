use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprFor {
  pub label: Label,
  pub pat: Pat,
  pub iter: Expr,
  pub body: Block,
  pub else_: Option<Block>,
}

impl ParsePratt<Expr, ExprFor> for Parser<'_> {} // todo
