use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Underscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplAuto;

impl<T> ParsePratt<T, Underscore> for Parser<'_> {
  fn _parse_initial(&mut self, _: Precedence) -> Result<Option<Underscore>, Diag> {
    if self.eat(Token::Underscore)? { Ok(Some(Underscore)) } else { Ok(None) }
  }
}

impl ParsePratt<Impl, ImplAuto> for Parser<'_> {
  fn _parse_initial(&mut self, _: Precedence) -> Result<Option<ImplAuto>, Diag> {
    if self.eat(Token::At)? { Ok(Some(ImplAuto)) } else { Ok(None) }
  }
}
