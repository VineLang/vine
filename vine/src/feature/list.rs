use super::*;

use crate::component::parser::ParseSequence;

#[derive_(Debug, Clone, Visit!)]
pub struct List<T> {
  pub children: Sequence<T>,
}

impl<T: ParseSequence> ParsePratt<T, List<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse_initial(&mut self, _: Precedence) -> Result<Option<List<T>>, Diag> {
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
