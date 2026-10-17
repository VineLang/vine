use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprCall {
  pub callee: Expr,
  pub args: Sequence<Expr>,
}

impl ParseKind<Expr, ExprCall> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TraitFn {
  pub callee: Ty,
  pub args: Sequence<Ty>,
  pub return_: Option<Ty>,
}

impl ParseKind<Trait, TraitFn> for Parser<'_> {} // todo
