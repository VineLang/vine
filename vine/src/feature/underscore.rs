use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprUnderscore {
  pub ty: Option<Ty>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct TyUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplUnderscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplAuto;
