use super::*;

#[derive_(Debug, Clone, Copy, Visit!)]
pub enum Mutable {
  Mut,
  Imm,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Loan<T> {
  pub mut_: Mutable,
  pub child: T,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Borrow<T> {
  pub star: Spanned<()>,
  pub mut_: Mutable,
  pub child: T,
}

impl Parser<'_> {
  fn parse_loan_kind(&mut self) -> Result<Option<Mutable>, Diag> {
    if self.eat(Token::Amp)? {
      Ok(Some(Mutable::Mut))
    } else if self.eat(Token::AmpAmp)? {
      Ok(Some(Mutable::Imm))
    } else {
      Ok(None)
    }
  }
}

impl<T> ParsePratt<T, Loan<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Loan<T>>, Diag> {
    if let Some(mut_) = self.parse_loan_kind()? {
      let child = self.parse_pratt(Precedence::Prefix)?;
      return Ok(Some(Loan { mut_, child }));
    }
    Ok(None)
  }

  fn _parse_dot(&mut self, initial: T) -> Result<Result<Loan<T>, T>, Diag> {
    if let Some(mut_) = self.parse_loan_kind()? {
      return Ok(Ok(Loan { mut_, child: initial }));
    }
    Ok(Err(initial))
  }
}

impl Parser<'_> {
  fn parse_borrow_kind(&mut self) -> Result<Option<(Spanned<()>, Mutable)>, Diag> {
    let span = self.start_span();
    if self.eat(Token::Star)? {
      Ok(Some((self.end_span(span).into(), Mutable::Mut)))
    } else if self.eat(Token::StarStar)? {
      Ok(Some((self.end_span(span).into(), Mutable::Imm)))
    } else {
      Ok(None)
    }
  }
}

impl<T> ParsePratt<T, Borrow<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Borrow<T>>, Diag> {
    if let Some((star, mut_)) = self.parse_borrow_kind()? {
      let child = self.parse_pratt(Precedence::Prefix)?;
      return Ok(Some(Borrow { star, mut_, child }));
    }
    Ok(None)
  }

  fn _parse_dot(&mut self, initial: T) -> Result<Result<Borrow<T>, T>, Diag> {
    if let Some((star, mut_)) = self.parse_borrow_kind()? {
      return Ok(Ok(Borrow { star, mut_, child: initial }));
    }
    Ok(Err(initial))
  }
}
