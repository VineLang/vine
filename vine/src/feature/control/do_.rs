use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprDo {
  pub label: Label,
  pub body: Block,
}


impl GrammarPrattPart for ExprDo {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
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
