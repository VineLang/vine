use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemConst {
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub ty: Ty,
  pub value: Option<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct AttrConfigurable;
