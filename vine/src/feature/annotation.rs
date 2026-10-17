use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprAnnotation {
  pub expr: Expr,
  pub ty: Ty,
}

impl ParseKind<Expr, ExprAnnotation> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatAnnotation {
  pub pat: Pat,
  pub ty: Ty,
}

impl ParseKind<Pat, PatAnnotation> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct ImplAnnotation {
  pub impl_: Impl,
  pub trait_: Trait,
}

impl ParseKind<Impl, ImplAnnotation> for Parser<'_> {} // todo
