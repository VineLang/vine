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

impl GrammarRule for ExprWhen {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ExprWhen",
      (
        Token::When,
        rule::<Label>().optional(),
        Token::OpenBrace,
        (rule::<WhenArm>(), Token::Comma.optional()).repeat(false),
        Token::CloseBrace,
        rule::<Block>().field("body"),
      ),
    )
  }
}

impl GrammarRule for WhenArm {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named("WhenArm", (below::<Expr>(Prec::Block), rule::<Block>()))
  }
}

impl Parse<ExprWhen> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprWhen>, Diag> {
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
    } else if let Some(cond) = self.try_parse_pratt(Prec::Block)? {
      let body = self.parse()?;
      self.eat(Token::Comma)?;
      Ok(Some(WhenArm { cond: Some(cond), body }))
    } else {
      Ok(None)
    }
  }
}
