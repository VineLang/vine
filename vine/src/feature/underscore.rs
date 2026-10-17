use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprUnderscore {
  pub ty: Option<Ty>,
}

impl ParsePratt<Expr, ExprUnderscore> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct TyUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplAuto;

impl ParsePratt<Pat, PatUnderscore> for Parser<'_> {} // todo
impl ParsePratt<Ty, TyUnderscore> for Parser<'_> {} // todo
impl ParsePratt<Impl, ImplUnderscore> for Parser<'_> {} // todo
impl ParsePratt<Impl, ImplAuto> for Parser<'_> {} // todo
