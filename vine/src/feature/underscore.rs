use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprUnderscore {
  pub ty: Option<Ty>,
}

impl ParseKind<Expr, ExprUnderscore> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct TyUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplAuto;

impl ParseKind<Pat, PatUnderscore> for Parser<'_> {} // todo
impl ParseKind<Ty, TyUnderscore> for Parser<'_> {} // todo
impl ParseKind<Impl, ImplUnderscore> for Parser<'_> {} // todo
impl ParseKind<Impl, ImplAuto> for Parser<'_> {} // todo
