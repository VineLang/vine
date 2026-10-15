use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct TyEnum {
  pub unsafe_: bool,
  pub variants: Sequence<TyVariant>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyVariant {
  pub tag: Spanned<Ident>,
  pub content: Sequence<Ty>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprVariant {
  pub tag: Spanned<Ident>,
  pub content: Sequence<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatVariant {
  pub tag: Spanned<Ident>,
  pub content: Sequence<Pat>,
}
