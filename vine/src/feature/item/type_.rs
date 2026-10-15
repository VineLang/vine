use super::*;

use crate::feature::flex::Flex;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemType {
  pub flex: Flex,
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub kind: ItemTypeKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemTypeKind {
  Opaque,
  Alias(Ty),
  Wrapper(Vis, Ty),
}
