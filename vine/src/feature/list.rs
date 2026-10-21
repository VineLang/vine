use super::*;

use crate::feature::sequence::{ParseSequence, SequenceChild};

#[derive_(Debug, Clone, Visit!)]
pub struct List<T> {
  pub children: Sequence<T>,
}


impl<T: ParseSequence> GrammarPrattKind for List<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*; // todo
  }
}

impl<T: ParseSequence> ParsePratt<T, List<T>> for Parser<'_>
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
