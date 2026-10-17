use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtAssert {
  pub cond: Expr,
  pub else_: Option<Block>,
}

impl ParsePratt<Stmt, StmtAssert> for Parser<'_> {} // todo
