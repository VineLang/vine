use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemImpl {
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub trait_: Trait,
  pub kind: ItemImplKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemImplKind {
  IndirectBasic,
  Indirect { impl_: Impl },
  Direct { items: Vec<ItemId> },
}
