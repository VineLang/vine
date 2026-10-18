use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtAssert {
  pub cond: Expr,
  pub else_: Option<Block>,
}

impl Parse<StmtAssert> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtAssert>, Diag> {
    if self.eat(Token::Assert)? {
      let cond = self.parse_pratt(Precedence::Min)?;
      let else_ = self.eat_then(Token::Else, Self::parse::<Block>)?;
      self.semi()?;
      Ok(Some(StmtAssert { cond, else_ }))
    } else {
      Ok(None)
    }
  }
}
