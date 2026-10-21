use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Inverse<T> {
  pub child: T,
}

impl<T: GrammarPratt> GrammarPrattKind for Inverse<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Prefix {
      rules.push(grammar.named("Inverse", (Token::Tilde, right::<T>(prec))));
    }
    if prec == Prec::Postfix && T::ALLOW_DOT {
      rules.push(grammar.named("DotInverse", (left::<T>(prec), Token::Dot, Token::Tilde)));
    }
  }
}

impl<T> ParsePratt<T, Inverse<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Inverse<T>>, Diag> {
    if self.eat(Token::Tilde)? {
      let child = self.parse_pratt(Prec::Prefix)?;
      return Ok(Some(Inverse { child }));
    }
    Ok(None)
  }

  fn _parse_dot(&mut self, initial: T) -> Result<Result<Inverse<T>, T>, Diag> {
    if self.eat(Token::Tilde)? {
      return Ok(Ok(Inverse { child: initial }));
    }
    Ok(Err(initial))
  }
}
