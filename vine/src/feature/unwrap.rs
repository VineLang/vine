use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprUnwrap {
  pub child: Expr,
  pub bang: Spanned<()>,
}

impl ParsePratt<Expr, ExprUnwrap> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TyUnwrap {
  pub child: Ty,
  pub bang: Spanned<()>,
}

impl ParsePratt<Ty, TyUnwrap> for Parser<'_> {} // todo
