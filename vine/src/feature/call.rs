use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprCall {
  pub callee: Expr,
  pub args: Sequence<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TraitFn {
  pub callee: Ty,
  pub args: Sequence<Ty>,
  pub return_: Option<Ty>,
}

impl ParsePratt<Expr, ExprCall> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    _: &mut PrecCtx,
    initial: Expr,
  ) -> Result<Result<ExprCall, Expr>, Diag> {
    if let Some(args) = self.try_parse::<Sequence<Expr>>()? {
      Ok(Ok(ExprCall { callee: initial, args }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl ParsePratt<Trait, TraitFn> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<TraitFn>, Diag> {
    if self.eat(Token::Fn)? {
      let callee = self.parse()?;
      let args = self.parse()?;
      let return_ = self.eat_then(Token::Type, Self::parse)?;
      Ok(Some(TraitFn { callee, args, return_ }))
    } else {
      Ok(None)
    }
  }
}
