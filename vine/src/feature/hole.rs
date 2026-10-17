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

impl ParseKind<Expr, ExprHole> for Parser<'_> {} // todo
impl ParseKind<Pat, PatHole> for Parser<'_> {} // todo
impl ParseKind<Ty, TyHole> for Parser<'_> {} // todo
impl ParseKind<Impl, ImplHole> for Parser<'_> {} // todo
impl ParseKind<Trait, TraitHole> for Parser<'_> {} // todo
