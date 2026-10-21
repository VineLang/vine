use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Read<T> {
  pub child: T,
}

impl<T: GrammarRule> GrammarPrattPart for Read<T> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![(Prec::Prefix, grammar.named("Read", (Token::At, right::<T>(Prec::Prefix))))]
  }
}

impl<T> ParsePratt<T, Read<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Read<T>>, Diag> {
    if self.eat(Token::At)? {
      let child = self.parse_pratt(Prec::Prefix)?;
      Ok(Some(Read { child }))
    } else {
      Ok(None)
    }
  }
}
