use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Underscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplAuto;

impl GrammarPratt for Underscore {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    vec![(Prec::Max, grammar.named("Underscore", Token::Underscore))]
  }
}

impl GrammarPratt for ImplAuto {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    vec![(Prec::Max, grammar.named("Auto", Token::At))]
  }
}

impl<T> ParsePratt<T, Underscore> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Underscore>, Diag> {
    if self.eat(Token::Underscore)? { Ok(Some(Underscore)) } else { Ok(None) }
  }
}

impl ParsePratt<Impl, ImplAuto> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<ImplAuto>, Diag> {
    if self.eat(Token::At)? { Ok(Some(ImplAuto)) } else { Ok(None) }
  }
}
