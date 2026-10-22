use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemTrait {
  pub inherent: GenericParams,
  pub name: Spanned<Ident>,
  pub generics: GenericParams,
  pub items: Children<Subitem>,
}

impl GrammarRule for ItemTrait {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ItemTrait",
      (
        Token::Trait,
        rule::<GenericParams>().field("inherent"),
        rule::<Ident>().field("name"),
        rule::<GenericParams>().field("generics").optional(),
        Token::OpenBrace,
        rule::<Item>().repeat(false),
        Token::CloseBrace,
      ),
    )
  }
}

impl Parse<ItemTrait> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ItemTrait>, Diag> {
    if self.eat(Token::Trait)? {
      let inherent = self.parse()?;
      let name = self.parse()?;
      let generics = self.parse()?;
      let items = self.parse_children(Some(Token::OpenBrace), None, Some(Token::CloseBrace))?;
      Ok(Some(ItemTrait { inherent, name, generics, items }))
    } else {
      Ok(None)
    }
  }
}
