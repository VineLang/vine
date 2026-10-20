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
  pub traits: Children<Path<!>>,
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

impl ParseAttr<AttrSelfDual> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrSelfDual>, Diag> {
    if name == "self_dual" { Ok(Some(AttrSelfDual)) } else { Ok(None) }
  }
}

impl ParseAttr<AttrImpl> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrImpl>, Diag> {
    if name == "impl" {
      let traits =
        self.parse_children(Some(Token::OpenParen), Some(Token::Comma), Some(Token::CloseParen))?;
      Ok(Some(AttrImpl { traits }))
    } else {
      Ok(None)
    }
  }
}
