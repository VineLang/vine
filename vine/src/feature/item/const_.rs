use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemConst {
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub ty: Option<Ty>,
  pub value: Option<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct AttrConfigurable;

impl Parse<ItemConst> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ItemConst>, Diag> {
    if self.eat(Token::Const)? {
      let name = self.parse()?;
      let generics = self.parse()?;
      let ty = self.eat_then(Token::Colon, Self::parse)?;
      let value = self.eat_then(Token::Eq, Self::parse)?;
      Ok(Some(ItemConst { name, generics, ty, value }))
    } else {
      Ok(None)
    }
  }
}

impl ParseAttr<AttrConfigurable> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrConfigurable>, Diag> {
    if name == "configurable" { Ok(Some(AttrConfigurable)) } else { Ok(None) }
  }
}
