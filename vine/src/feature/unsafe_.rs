use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Safe<T> {
  pub safe_: Spanned<()>,
  pub child: T,
}

impl<T> ParsePratt<T, Safe<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Safe<T>>, Diag> {
    let safe_ = self.token_span();
    if self.eat(Token::Safe)? {
      let child = self.parse_pratt(Prec::Prefix)?;
      Ok(Some(Safe { safe_, child }))
    } else {
      Ok(None)
    }
  }
}
