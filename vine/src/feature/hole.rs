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

impl ParsePratt<Expr, ExprHole> for Parser<'_> {} // todo
impl ParsePratt<Pat, PatHole> for Parser<'_> {} // todo
impl ParsePratt<Ty, TyHole> for Parser<'_> {} // todo
impl ParsePratt<Impl, ImplHole> for Parser<'_> {} // todo
impl ParsePratt<Trait, TraitHole> for Parser<'_> {} // todo
