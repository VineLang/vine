use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtAssert {
  pub cond: Expr,
  pub else_: Option<Block>,
}

impl GrammarRule for StmtAssert {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "StmtAssert",
      (
        Token::Assert,
        below::<Expr>(Prec::Block),
        choice(((Token::Else, rule::<Block>().field("else"), Token::Semi.optional()), Token::Semi)),
      ),
    )
  }
}

impl Parse<StmtAssert> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtAssert>, Diag> {
    if self.eat(Token::Assert)? {
      let cond = self.parse_pratt(Prec::Block)?;
      let else_ = self.eat_then(Token::Else, Self::parse::<Block>)?;
      self.semi(else_.is_none())?;
      Ok(Some(StmtAssert { cond, else_ }))
    } else {
      Ok(None)
    }
  }
}
