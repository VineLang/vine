use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprLoop {
  pub label: Label,
  pub body: Block,
}

impl GrammarRule for ExprLoop {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ExprLoop",
      (
        Token::Loop,
        rule::<Label>().optional(),
        below::<Expr>(Prec::Block),
        rule::<Block>().field("body"),
      ),
    )
  }
}

impl Parse<ExprLoop> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprLoop>, Diag> {
    if self.eat(Token::Loop)? {
      let label = self.parse()?;
      let body = self.parse()?;
      Ok(Some(ExprLoop { label, body }))
    } else {
      Ok(None)
    }
  }
}
