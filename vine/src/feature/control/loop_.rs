use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprLoop {
  pub label: Label,
  pub body: Block,
}

impl ParsePratt<Expr, ExprLoop> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<ExprLoop>, Diag> {
    if self.eat(Token::Loop)? {
      let label = self.parse()?;
      let body = self.parse()?;
      Ok(Some(ExprLoop { label, body }))
    } else {
      Ok(None)
    }
  }
}
