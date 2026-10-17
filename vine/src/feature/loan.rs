use super::*;

#[derive_(Debug, Clone, Copy, Visit!)]
pub enum Mutable {
  Mut,
  Imm,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprLoan {
  pub mut_: Mutable,
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatLoan {
  pub mut_: Mutable,
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyLoan {
  pub mut_: Mutable,
  pub child: Ty,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprBorrow {
  pub star: Spanned<()>,
  pub mut_: Mutable,
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatBorrow {
  pub star: Spanned<()>,
  pub mut_: Mutable,
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyBorrow {
  pub star: Spanned<()>,
  pub mut_: Mutable,
  pub child: Ty,
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

impl ParseKind<Expr, ExprLoan> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<ExprLoan>, Diag> {
    if let Some(mut_) = self.parse_loan_kind()? {
      let child = self.parse_expr(Precedence::Prefix)?;
      return Ok(Some(ExprLoan { mut_, child }));
    }
    Ok(None)
  }
}

impl ParseKind<Pat, PatLoan> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<PatLoan>, Diag> {
    if let Some(mut_) = self.parse_loan_kind()? {
      let child = self.parse_pat(Precedence::Prefix)?;
      return Ok(Some(PatLoan { mut_, child }));
    }
    Ok(None)
  }
}

impl ParseKind<Ty, TyLoan> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<TyLoan>, Diag> {
    if let Some(mut_) = self.parse_loan_kind()? {
      let child = self.parse_ty(Precedence::Prefix)?;
      return Ok(Some(TyLoan { mut_, child }));
    }
    Ok(None)
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

impl ParseKind<Expr, ExprBorrow> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<ExprBorrow>, Diag> {
    if let Some((star, mut_)) = self.parse_borrow_kind()? {
      let child = self.parse_expr(Precedence::Prefix)?;
      return Ok(Some(ExprBorrow { star, mut_, child }));
    }
    Ok(None)
  }
}

impl ParseKind<Pat, PatBorrow> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<PatBorrow>, Diag> {
    if let Some((star, mut_)) = self.parse_borrow_kind()? {
      let child = self.parse_pat(Precedence::Prefix)?;
      return Ok(Some(PatBorrow { star, mut_, child }));
    }
    Ok(None)
  }
}

impl ParseKind<Ty, TyBorrow> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<TyBorrow>, Diag> {
    if let Some((star, mut_)) = self.parse_borrow_kind()? {
      let child = self.parse_ty(Precedence::Prefix)?;
      return Ok(Some(TyBorrow { star, mut_, child }));
    }
    Ok(None)
  }
}
