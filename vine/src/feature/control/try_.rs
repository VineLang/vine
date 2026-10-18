use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprTry {
  pub result: Expr,
  pub try_: Spanned<()>,
}

impl ParsePratt<Expr, ExprTry> for Parser<'_> {
  fn _parse_dot(&mut self, initial: Expr) -> Result<Result<ExprTry, Expr>, Diag> {
    let try_ = self.token_span();
    if self.eat(Token::Try)? { Ok(Ok(ExprTry { result: initial, try_ })) } else { Ok(Err(initial)) }
  }
}
