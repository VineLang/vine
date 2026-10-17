use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprField {
  pub expr: Expr,
  pub field: Field,
}

impl ParseKind<Expr, ExprField> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TyField {
  pub ty: Ty,
  pub field: Field,
}

impl ParseKind<Ty, TyField> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub enum Field {
  Tuple(Span, usize),
  Object(Spanned<Ident>),
}
