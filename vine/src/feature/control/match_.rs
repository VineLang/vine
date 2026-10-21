use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprMatch {
  pub label: Label,
  pub value: Expr,
  pub arms: Children<MatchArm>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct MatchArm {
  pub pat: Pat,
  pub body: Block,
}


impl GrammarPrattKind for ExprMatch {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*; // todo
  }
}

impl ParsePratt<Expr, ExprMatch> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<ExprMatch>, Diag> {
    if self.eat(Token::Match)? {
      let label = self.parse()?;
      let value = self.parse_pratt(Prec::Cond)?;
      let arms = self.parse_children(Some(Token::OpenBrace), None, Some(Token::CloseBrace))?;
      Ok(Some(ExprMatch { label, value, arms }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<MatchArm> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<MatchArm>, Diag> {
    if let Some(pat) = self.try_parse_pratt(Prec::Cond)? {
      let body = self.parse()?;
      self.eat(Token::Comma)?;
      Ok(Some(MatchArm { pat, body }))
    } else {
      Ok(None)
    }
  }
}
