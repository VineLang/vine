use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Annotation<C, T> {
  pub child: C,
  pub ty: T,
}

impl<C: GrammarPratt, T: GrammarPratt> GrammarPrattKind for Annotation<C, T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Annotation {
      rules.push(grammar.named("Annotation", (left::<C>(prec), Token::Colon, right::<T>(prec))))
    }
  }
}

impl<C, T> ParsePratt<C, Annotation<C, T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: C,
  ) -> Result<Result<Annotation<C, T>, C>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Annotation)
      && self.eat(Token::Colon)?
    {
      let ty = self.parse_pratt(prec())?;
      Ok(Ok(Annotation { child: initial, ty }))
    } else {
      Ok(Err(initial))
    }
  }
}
