use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Safe<T> {
  pub safe_: Spanned<()>,
  pub child: T,
}

impl<T: GrammarPratt> GrammarPrattKind for Safe<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Prefix {
      rules.push(grammar.named("Safe", (Token::Safe, right::<T>(prec))))
    }
  }
}

impl<T> ParsePratt<T, Safe<T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Safe<T>>, Diag> {
    if let Some(safe_) = self.eat_spanned(Token::Safe)? {
      let child = self.parse_pratt(Prec::Prefix)?;
      Ok(Some(Safe { safe_, child }))
    } else {
      Ok(None)
    }
  }
}
