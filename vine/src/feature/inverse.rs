use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Inverse<T> {
  pub child: T,
}

impl<T> ParsePratt<T, Inverse<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Inverse<T>>, Diag> {
    if self.eat(Token::Tilde)? {
      let child = self.parse_pratt(Precedence::Prefix)?;
      return Ok(Some(Inverse { child }));
    }
    Ok(None)
  }

  fn _parse_dot(&mut self, initial: T) -> Result<Result<Inverse<T>, T>, Diag> {
    if self.eat(Token::Tilde)? {
      return Ok(Ok(Inverse { child: initial }));
    }
    Ok(Err(initial))
  }
}
