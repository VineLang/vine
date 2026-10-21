use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Underscore;

#[derive_(Debug, Clone, Visit!)]
pub struct ImplAuto;

impl GrammarPrattKind for Underscore {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    if prec == Prec::Max {
      rules.push(grammar.named("Underscore", Token::Underscore));
    }
  }
}

impl GrammarPrattKind for ImplAuto {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    if prec == Prec::Max {
      rules.push(grammar.named("Auto", Token::At));
    }
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
