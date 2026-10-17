use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprSafe {
  pub safe_: Spanned<()>,
  pub child: Expr,
}

impl ParseKind<Expr, ExprSafe> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatSafe {
  pub safe_: Spanned<()>,
  pub child: Pat,
}

impl ParseKind<Pat, PatSafe> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct ImplSafe {
  pub safe_: Spanned<()>,
  pub child: Impl,
}
