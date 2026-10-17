use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprCall {
  pub callee: Expr,
  pub args: Sequence<Expr>,
}

impl ParsePratt<Expr, ExprCall> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TraitFn {
  pub callee: Ty,
  pub args: Sequence<Ty>,
  pub return_: Option<Ty>,
}

impl ParsePratt<Trait, TraitFn> for Parser<'_> {} // todo
