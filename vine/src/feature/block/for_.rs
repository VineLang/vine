use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprFor {
  pub label: Label,
  pub pat: Pat,
  pub iter: Expr,
  pub body: Block,
  pub else_: Option<Block>,
}

impl GrammarRule for ExprFor {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ExprFor",
      (
        Token::For,
        rule::<Label>().optional(),
        below::<Pat>(Prec::Comparison),
        Token::In,
        below::<Expr>(Prec::Comparison),
        rule::<Block>().field("body"),
        (Token::Else, rule::<Block>().field("else")).optional(),
      ),
    )
  }
}

impl Parse<ExprFor> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprFor>, Diag> {
    if self.eat(Token::For)? {
      let label = self.parse()?;
      let pat = self.parse_pratt(Prec::Comparison)?;
      self.expect(Token::In)?;
      let iter = self.parse_pratt(Prec::Comparison)?;
      let body = self.parse()?;
      let else_ = self.eat_then(Token::Else, Self::parse)?;
      Ok(Some(ExprFor { label, pat, iter, body, else_ }))
    } else {
      Ok(None)
    }
  }
}
