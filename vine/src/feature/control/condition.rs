use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprIs {
  pub expr: Expr,
  pub pat: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprAny {
  pub conds: Children<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprAll {
  pub conds: Children<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprImplies {
  pub asusmption: Expr,
  pub conclusion: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprNot {
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatIs {
  pub bind: Pat,
  pub pat: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatAny {
  pub conds: Children<Pat>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatAll {
  pub conds: Children<Pat>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatNot {
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatCond {
  pub pat: Pat,
  pub cond: Expr,
}
