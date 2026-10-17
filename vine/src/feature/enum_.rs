use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct TyEnum {
  pub unsafe_: bool,
  pub variants: Sequence<TyVariant>,
}

impl ParseKind<Ty, TyEnum> for Parser<'_> {} // todo

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

impl ParseKind<Expr, ExprVariant> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatVariant {
  pub tag: Spanned<Ident>,
  pub content: Sequence<Pat>,
}

impl ParseKind<Pat, PatVariant> for Parser<'_> {} // todo
