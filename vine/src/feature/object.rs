use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprObject {
  pub children: Object<Expr>,
}

impl ParsePratt<Expr, ExprObject> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatObject {
  pub children: Object<Pat>,
}

impl ParsePratt<Pat, PatObject> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TyObject {
  pub children: Object<Ty>,
}

impl ParsePratt<Ty, TyObject> for Parser<'_> {} // todo
