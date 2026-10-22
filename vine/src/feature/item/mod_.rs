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

impl GrammarRule for ItemMod {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ItemMod",
      (
        Token::Mod,
        choice((
          (rule::<Ident>().field("name").optional(), Token::Semi),
          (
            rule::<Ident>().field("name").optional(),
            rule::<GenericParams>().field("generics").optional(),
            Token::OpenBrace,
            rule::<Item>().repeat(false),
            Token::CloseBrace,
          ),
        )),
      ),
    )
  }
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
