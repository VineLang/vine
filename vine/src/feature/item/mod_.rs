use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemMod {
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub kind: ItemModKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ItemModKind {
  Local(Children<Subitem>),
  External,
}

impl Parse<Vis> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Vis>, Diag> {
    if self.eat(Token::Pub)? {
      if self.eat(Token::Colon)? {
        Ok(Some(Vis::Within(self.parse()?)))
      } else {
        Ok(Some(Vis::Public))
      }
    } else {
      Ok(Some(Vis::Private))
    }
  }
}

impl Parse<Subitem> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Subitem>, Diag> {
    todo!() // todo
  }
}
