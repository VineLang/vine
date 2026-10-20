use super::*;

use crate::feature::{children::ChildrenEntry, sequence::ParseSequence};

#[derive_(Debug, Clone, Visit!)]
pub struct TyEnum {
  pub unsafe_: bool,
  pub variants: Children<Variant<Ty>>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Variant<T> {
  pub tag: Spanned<Ident>,
  pub content: Sequence<T>,
}


impl GrammarPratt for TyEnum {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Ty, TyEnum> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<TyEnum>, Diag> {
    let unsafe_ = self.eat(Token::Unsafe)?;
    if let Some(initial) = self.try_parse::<Variant<Ty>>()? {
      let mut variants = self
        .eat_then(Token::Or, |self_| self_.parse_children(None, Some(Token::Or), None))?
        .unwrap_or_default();
      variants.entries.insert(0, ChildrenEntry::from(initial));
      Ok(Some(TyEnum { unsafe_, variants }))
    } else if self.eat(Token::Any)? {
      let variants =
        self.parse_children(Some(Token::OpenBrace), Some(Token::Comma), Some(Token::CloseBrace))?;
      Ok(Some(TyEnum { unsafe_, variants }))
    } else if unsafe_ {
      self.unexpected()
    } else {
      Ok(None)
    }
  }
}


impl<T: ParseSequence> GrammarPratt for Variant<T> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl<T: ParseSequence> ParsePratt<T, Variant<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Variant<T>>, Diag> {
    self.try_parse::<Variant<T>>()
  }
}

impl<T: ParseSequence> Parse<Variant<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse(&mut self) -> Result<Option<Variant<T>>, Diag> {
    if self.eat(Token::Dot)? {
      let tag = self.parse::<Spanned<Ident>>()?;
      let content = self.try_parse::<Sequence<T>>()?.unwrap_or_default();
      Ok(Some(Variant { tag, content }))
    } else {
      Ok(None)
    }
  }
}
