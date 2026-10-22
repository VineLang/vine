use super::*;

use crate::feature::sequence::{SequenceChild, SequenceKind};

#[derive_(Debug, Clone, Visit!)]
pub struct List<T> {
  pub children: Sequence<T>,
}

impl<T: SequenceKind + GrammarPratt> GrammarPrattKind for List<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Max {
      rules.push(grammar.named(
        "List",
        delimited::<SequenceChild<T>>(Token::OpenBracket, Token::Comma, Token::CloseBracket),
      ))
    }
  }
}

impl<T: SequenceKind> ParsePratt<T, List<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<List<T>>, Diag> {
    if self.eat(Token::OpenBracket)? {
      let children = self.parse_children::<SequenceChild<T>>(
        None,
        Some(Token::Comma),
        Some(Token::CloseBracket),
      )?;
      Ok(Some(List { children }))
    } else {
      Ok(None)
    }
  }
}
