use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemImpl<I> {
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub trait_: Trait,
  pub kind: ItemImplKind<I>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemImplKind<I> {
  IndirectBasic,
  Indirect { impl_: Impl },
  Direct { items: Vec<I> },
}

#[derive_(Debug, Clone, Visit!)]
pub struct AttrBasic;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrManual;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrBecome {
  pub path: Path,
}
