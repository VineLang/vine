use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Unwrap<T> {
  pub child: T,
  pub bang: Spanned<()>,
}

impl<T: GrammarPratt> GrammarPrattKind for Unwrap<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Postfix {
      rules.push(grammar.named("Unwrap", (left::<T>(Prec::Postfix), Token::Bang)));
    }
  }
}

impl<T> ParsePratt<T, Unwrap<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_suffix(&mut self, _: &mut PrecCtx, initial: T) -> Result<Result<Unwrap<T>, T>, Diag> {
    if let Some(bang) = self.eat_spanned(Token::Bang)? {
      Ok(Ok(Unwrap { child: initial, bang }))
    } else {
      Ok(Err(initial))
    }
  }
}
