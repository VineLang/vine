use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprIs {
  pub expr: Expr,
  pub pat: Pat,
}

impl ParsePratt<Expr, ExprIs> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct ExprAny {
  pub conds: Children<Expr>,
}

impl ParsePratt<Expr, ExprAny> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct ExprAll {
  pub conds: Children<Expr>,
}

impl ParsePratt<Expr, ExprAll> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct ExprImplies {
  pub asusmption: Expr,
  pub conclusion: Expr,
}

impl ParsePratt<Expr, ExprImplies> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct ExprNot {
  pub child: Expr,
}

impl ParsePratt<Expr, ExprNot> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatIs {
  pub bind: Pat,
  pub pat: Pat,
}

impl ParsePratt<Pat, PatIs> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatAny {
  pub conds: Children<Pat>,
}

impl ParsePratt<Pat, PatAny> for Parser<'_> {} // todo

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

impl ParsePratt<Pat, PatCond> for Parser<'_> {} // todo
