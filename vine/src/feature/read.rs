use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprRead {
  pub child: Expr,
}

impl ParseKind<Expr, ExprRead> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatRead {
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyRead {
  pub child: Ty,
}
