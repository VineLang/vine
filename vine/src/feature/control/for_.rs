use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprFor {
  pub label: Label,
  pub pat: Pat,
  pub iter: Expr,
  pub body: Block,
  pub else_: Option<Block>,
}

impl ParsePratt<Expr, ExprFor> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<ExprFor>, Diag> {
    if self.eat(Token::For)? {
      let label = self.parse()?;
      let pat = self.parse_pratt(Precedence::Comparison)?;
      self.expect(Token::In)?;
      let iter = self.parse_pratt(Precedence::Comparison)?;
      let body = self.parse()?;
      let else_ = self.eat_then(Token::Else, Self::parse)?;
      Ok(Some(ExprFor { label, pat, iter, body, else_ }))
    } else {
      Ok(None)
    }
  }
}
