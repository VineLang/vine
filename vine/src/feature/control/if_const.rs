use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprIfConst {
  pub label: Label,
  pub cond: Path,
  pub then: Block,
  pub else_: Option<Block>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyIfConst {
  pub cond: Path,
  pub then: Ty,
  pub else_: Option<Ty>,
}
