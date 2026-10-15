use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprField {
  pub expr: Expr,
  pub field: Field,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyField {
  pub ty: Ty,
  pub field: Field,
}

#[derive_(Debug, Clone, Visit!)]
pub enum Field {
  Tuple(Span, usize),
  Object(Spanned<Ident>),
}
