use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprLoop {
  pub label: Label,
  pub body: Block,
}

impl ParseKind<Expr, ExprLoop> for Parser<'_> {} // todo
