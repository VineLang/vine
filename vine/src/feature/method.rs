use super::*;

use crate::feature::generics::GenericParams;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprMethod {
  pub recv: Expr,
  pub method: Spanned<Ident>,
  pub generics: GenericParams,
  pub args: Sequence<Expr>,
}

// `_parse_dot` handled by `ParsePratt<Expr, ExprKind>`
impl ParsePratt<Expr, ExprMethod> for Parser<'_> {}
