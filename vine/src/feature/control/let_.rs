use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtLet {
  pub pat: Pat,
  pub kind: LetKind,
}

impl ParsePratt<Stmt, StmtLet> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub enum LetKind {
  Uninit,
  Init(Expr),
  Loop,
}
