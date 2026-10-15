use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemMod {
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub kind: ItemModKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemModKind {
  Local(Vec<ItemId>),
  External,
}
