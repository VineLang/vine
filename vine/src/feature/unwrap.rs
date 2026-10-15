use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprUnwrap {
  pub child: Expr,
  pub bang: Spanned<()>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyUnwrap {
  pub child: Ty,
  pub bang: Spanned<()>,
}
