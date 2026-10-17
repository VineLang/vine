use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprDo {
  pub label: Label,
  pub body: Block,
}

impl ParseKind<Expr, ExprDo> for Parser<'_> {} // todo
