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

impl GrammarRule for ExprMatch {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ExprMatch",
      (
        Token::Match,
        rule::<Label>().optional(),
        below::<Expr>(Prec::Block),
        Token::OpenBrace,
        (rule::<MatchArm>(), Token::Comma.optional()).repeat(false),
        Token::CloseBrace,
        rule::<Block>().field("body"),
      ),
    )
  }
}

impl GrammarRule for MatchArm {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named("MatchArm", (below::<Pat>(Prec::Block), rule::<Block>()))
  }
}

impl Parse<ExprMatch> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprMatch>, Diag> {
    if self.eat(Token::Match)? {
      let label = self.parse()?;
      let value = self.parse_pratt(Prec::Block)?;
      let arms = self.parse_children(Some(Token::OpenBrace), None, Some(Token::CloseBrace))?;
      Ok(Some(ExprMatch { label, value, arms }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<MatchArm> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<MatchArm>, Diag> {
    if let Some(pat) = self.try_parse_pratt(Prec::Block)? {
      let body = self.parse()?;
      self.eat(Token::Comma)?;
      Ok(Some(MatchArm { pat, body }))
    } else {
      Ok(None)
    }
  }
}
