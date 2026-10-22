use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprDo {
  pub label: Label,
  pub body: Block,
}

impl GrammarRule for ExprDo {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named("ExprDo", (Token::Do, rule::<Label>().optional(), rule::<Block>().field("body")))
  }
}

impl Parse<ExprDo> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprDo>, Diag> {
    if self.eat(Token::Do)? {
      let label = self.parse()?;
      let body = self.parse()?;
      Ok(Some(ExprDo { label, body }))
    } else {
      Ok(None)
    }
  }
}
