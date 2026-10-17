use super::*;

use crate::feature::generics::GenericParams;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprMethod {
  pub recv: Expr,
  pub method: Spanned<Ident>,
  pub generics: GenericParams,
  pub args: Sequence<Expr>,
}

impl ParsePratt<Expr, ExprMethod> for Parser<'_> {} // todo
