use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprTuple {
  pub children: Sequence<Expr>,
}

impl ParseKind<Expr, ExprTuple> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatTuple {
  pub children: Sequence<Pat>,
}

impl ParseKind<Pat, PatTuple> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TyTuple {
  pub children: Sequence<Ty>,
}

impl ParseKind<Ty, TyTuple> for Parser<'_> {} // todo
