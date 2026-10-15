use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtHole;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprHole;

#[derive_(Debug, Clone, Visit!)]
pub struct PatHole;

#[derive_(Debug, Clone, Visit!)]
pub struct TyHole;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplHole;

#[derive_(Debug, Clone, Visit!)]
pub struct TraitHole;
