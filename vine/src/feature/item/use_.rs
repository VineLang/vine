use super::*;

use crate::feature::path::PathComponent;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemUse {
  pub imports: Children<ImportTree>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ImportTree {
  pub span: LocalSpan,
  pub component: PathComponent,
  pub children: Option<Children<ImportChild>>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ImportChild {
  Tree(ImportTree),
  /// `as <Ident>`
  Alias(Spanned<Ident>),
  /// `as _`
  Implicit,
  /// `*`
  Glob,
  /// `* as _`
  GlobImplicit,
}

impl GrammarRule for ItemUse {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*; // todo
    grammar.named("ItemUse", (Token::Use, rule::<Path<!>>(), Token::Semi))
  }
}

impl Parse<ItemUse> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ItemUse>, Diag> {
    if self.eat(Token::Use)? {
      let imports = if let Some(tree) = self.try_parse::<ImportTree>()? {
        Children::single(tree)
      } else {
        self.parse_children(Some(Token::OpenBrace), Some(Token::Comma), Some(Token::CloseBrace))?
      };
      self.semi(true)?;
      Ok(Some(ItemUse { imports }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<ImportTree> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ImportTree>, Diag> {
    let span = self.start_span();
    if let Some(component) = self.try_parse::<PathComponent>()? {
      let children = if self.eat(Token::ColonColon)? {
        if let Some(child) = self.try_parse::<ImportChild>()? {
          Some(Children::single(child))
        } else {
          Some(self.parse_children(
            Some(Token::OpenBrace),
            Some(Token::Comma),
            Some(Token::CloseBrace),
          )?)
        }
      } else if self.check(Token::As) {
        Some(Children::single(self.parse::<ImportChild>()?))
      } else {
        None
      };
      let span = self.end_span(span);
      Ok(Some(ImportTree { span, component, children }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<ImportChild> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ImportChild>, Diag> {
    if let Some(tree) = self.try_parse::<ImportTree>()? {
      Ok(Some(ImportChild::Tree(tree)))
    } else if self.eat(Token::As)? {
      if let Some(alias) = self.try_parse::<Spanned<Ident>>()? {
        Ok(Some(ImportChild::Alias(alias)))
      } else {
        self.expect(Token::Underscore)?;
        Ok(Some(ImportChild::Implicit))
      }
    } else if self.eat(Token::Star)? {
      if self.eat(Token::As)? {
        self.expect(Token::Underscore)?;
        Ok(Some(ImportChild::GlobImplicit))
      } else {
        Ok(Some(ImportChild::Glob))
      }
    } else {
      Ok(None)
    }
  }
}
