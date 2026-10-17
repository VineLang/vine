use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprInverse {
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatInverse {
  pub child: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyInverse {
  pub child: Ty,
}

impl ParseKind<Expr, ExprInverse> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<ExprInverse>, Diag> {
    if self.eat(Token::Tilde)? {
      let child = self.parse_expr(Precedence::Prefix)?;
      return Ok(Some(ExprInverse { child }));
    }
    Ok(None)
  }
}

impl ParseKind<Pat, PatInverse> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<PatInverse>, Diag> {
    if self.eat(Token::Tilde)? {
      let child = self.parse_pat(Precedence::Prefix)?;
      return Ok(Some(PatInverse { child }));
    }
    Ok(None)
  }
}

impl ParseKind<Ty, TyInverse> for Parser<'_> {
  fn _parse_initial(&mut self) -> Result<Option<TyInverse>, Diag> {
    if self.eat(Token::Tilde)? {
      let child = self.parse_ty(Precedence::Prefix)?;
      return Ok(Some(TyInverse { child }));
    }
    Ok(None)
  }
}
