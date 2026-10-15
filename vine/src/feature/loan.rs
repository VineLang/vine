use super::*;

#[derive_(Debug, Clone, Copy, Visit!)]
pub enum Mutable {
  Mut,
  Imm,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprLoan {
  pub mut_: Mutable,
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatLoan {
  pub mut_: Mutable,
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyLoan {
  pub mut_: Mutable,
  pub child: Ty,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprBorrow {
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatBorrow {
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyBorrow {
  pub child: Ty,
}
