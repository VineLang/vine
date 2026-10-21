use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Annotation<T> {
  pub child: T,
  pub ty: Ty,
}


impl<T> GrammarPrattKind for Annotation<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*; // todo
  }
}

impl<T> ParsePratt<T, Annotation<T>> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: T,
  ) -> Result<Result<Annotation<T>, T>, Diag> {
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
