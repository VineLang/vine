use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemMod<I> {
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub kind: ItemModKind<I>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemModKind<I> {
  Local(Vec<I>),
  External,
}
