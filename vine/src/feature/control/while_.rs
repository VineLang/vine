use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprWhile {
  pub label: Label,
  pub cond: Expr,
  pub body: Block,
  pub else_: Option<Block>,
}

impl ParsePratt<Expr, ExprWhile> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<ExprWhile>, Diag> {
    if self.eat(Token::While)? {
      let label = self.parse()?;
      let cond = self.parse_pratt(Prec::Cond)?;
      let body = self.parse()?;
      let else_ = self.eat_then(Token::Else, Self::parse)?;
      Ok(Some(ExprWhile { label, cond, body, else_ }))
    } else {
      Ok(None)
    }
  }
}
