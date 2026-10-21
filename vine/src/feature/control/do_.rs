use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprDo {
  pub label: Label,
  pub body: Block,
}

impl GrammarPrattKind for ExprDo {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*; // todo
  }
}

impl ParsePratt<Expr, ExprDo> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<ExprDo>, Diag> {
    if self.eat(Token::Do)? {
      let label = self.parse()?;
      let body = self.parse()?;
      Ok(Some(ExprDo { label, body }))
    } else {
      Ok(None)
    }
  }
}
