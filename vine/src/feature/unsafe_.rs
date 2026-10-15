use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprSafe {
  pub safe_: Spanned<()>,
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatSafe {
  pub safe_: Spanned<()>,
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ImplSafe {
  pub safe_: Spanned<()>,
  pub child: Impl,
}
