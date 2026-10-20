use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemImpl {
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub safe: Option<Spanned<()>>,
  pub trait_: Trait,
  pub kind: ItemImplKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemImplKind {
  IndirectBasic,
  Indirect { impl_: Impl },
  Direct { items: Children<Subitem> },
}

#[derive_(Debug, Clone, Visit!)]
pub struct AttrBasic;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrManual;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrBecome {
  pub path: Path<!>,
}

impl Parse<ItemImpl> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ItemImpl>, Diag> {
    if self.eat(Token::Impl)? {
      let name = self.parse()?;
      let generics = self.parse()?;
      self.expect(Token::Colon)?;
      let safe = self.eat_spanned(Token::Safe)?;
      let trait_ = self.parse()?;
      let kind = if self.eat(Token::OpenBrace)? {
        ItemImplKind::Direct { items: self.parse_children(None, None, Some(Token::CloseBrace))? }
      } else if self.eat(Token::Eq)? {
        ItemImplKind::Indirect { impl_: self.parse()? }
      } else {
        ItemImplKind::IndirectBasic
      };
      self.semi(!matches!(kind, ItemImplKind::Direct { .. }))?;
      Ok(Some(ItemImpl { name, generics, safe, trait_, kind }))
    } else {
      Ok(None)
    }
  }
}

impl ParseAttr<AttrBasic> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrBasic>, Diag> {
    if name == "basic" { Ok(Some(AttrBasic)) } else { Ok(None) }
  }
}

impl ParseAttr<AttrManual> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrManual>, Diag> {
    if name == "manual" { Ok(Some(AttrManual)) } else { Ok(None) }
  }
}

impl ParseAttr<AttrBecome> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrBecome>, Diag> {
    if name == "become" {
      self.expect(Token::OpenParen)?;
      let path = self.parse()?;
      self.expect(Token::CloseParen)?;
      Ok(Some(AttrBecome { path }))
    } else {
      Ok(None)
    }
  }
}
