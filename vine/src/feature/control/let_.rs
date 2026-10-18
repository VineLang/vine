use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtLet {
  pub pat: Pat,
  pub kind: LetKind,
}

// Handled directly by `Parse<StmtKind>`
impl Parse<StmtLet> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtLet>, Diag> {
    Ok(None)
  }
}

#[derive_(Debug, Clone, Visit!)]
pub enum LetKind {
  Uninit,
  Init(Expr),
  Loop,
}
