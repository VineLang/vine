use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Read<T> {
  pub child: T,
}

impl<T> ParsePratt<T, Read<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: Precedence) -> Result<Option<Read<T>>, Diag> {
    if self.eat(Token::At)? {
      let child = self.parse_pratt(Precedence::Prefix)?;
      Ok(Some(Read { child }))
    } else {
      Ok(None)
    }
  }
}
