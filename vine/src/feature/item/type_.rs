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

#[derive_(Debug, Clone, Visit!)]
pub struct AttrSelfDual;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrImpl {
  pub traits: Sequence<Path>,
}

impl Parse<ItemType> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ItemType>, Diag> {
    if self.eat(Token::Type)? {
      let flex = self.parse()?;
      let name = self.parse()?;
      let generics = self.parse()?;
      let kind = if self.eat(Token::Colon)? {
        ItemTypeKind::Wrapper(self.parse()?, self.parse()?)
      } else if self.eat(Token::Eq)? {
        ItemTypeKind::Alias(self.parse()?)
      } else {
        ItemTypeKind::Opaque
      };
      self.semi(true)?;
      Ok(Some(ItemType { flex, name, generics, kind }))
    } else {
      Ok(None)
    }
  }
}
