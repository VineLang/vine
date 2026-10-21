use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprLoop {
  pub label: Label,
  pub body: Block,
}


impl GrammarPrattKind for ExprLoop {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*; // todo
  }
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
