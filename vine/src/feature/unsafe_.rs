use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprSafe {
  pub safe_: Spanned<()>,
  pub child: Expr,
}

impl ParsePratt<Expr, ExprSafe> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatSafe {
  pub safe_: Spanned<()>,
  pub child: Pat,
}

impl ParsePratt<Pat, PatSafe> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct ImplSafe {
  pub safe_: Spanned<()>,
  pub child: Impl,
}
