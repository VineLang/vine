use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprWhile {
  pub label: Label,
  pub cond: Expr,
  pub body: Block,
  pub else_: Option<Block>,
}

impl GrammarRule for ExprWhile {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ExprWhile",
      (
        Token::While,
        rule::<Label>().optional(),
        below::<Expr>(Prec::Block),
        rule::<Block>().field("body"),
        (Token::Else, rule::<Block>().field("else")).optional(),
      ),
    )
  }
}

impl Parse<ExprWhile> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprWhile>, Diag> {
    if self.eat(Token::While)? {
      let label = self.parse()?;
      let cond = self.parse_pratt(Prec::Block)?;
      let body = self.parse()?;
      let else_ = self.eat_then(Token::Else, Self::parse)?;
      Ok(Some(ExprWhile { label, cond, body, else_ }))
    } else {
      Ok(None)
    }
  }
}
