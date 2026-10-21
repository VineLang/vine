use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprWhen {
  pub label: Label,
  pub arms: Children<WhenArm>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct WhenArm {
  pub cond: Option<Expr>,
  pub body: Block,
}


impl GrammarPrattPart for ExprWhen {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Expr, ExprWhen> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<ExprWhen>, Diag> {
    if self.eat(Token::When)? {
      let label = self.parse()?;
      let arms = self.parse_children(Some(Token::OpenBrace), None, Some(Token::CloseBrace))?;
      Ok(Some(ExprWhen { label, arms }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<WhenArm> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<WhenArm>, Diag> {
    if self.eat(Token::Underscore)? {
      let body = self.parse()?;
      Ok(Some(WhenArm { cond: None, body }))
    } else if let Some(cond) = self.try_parse_pratt(Prec::Cond)? {
      let body = self.parse()?;
      self.eat(Token::Comma)?;
      Ok(Some(WhenArm { cond: Some(cond), body }))
    } else {
      Ok(None)
    }
  }
}
