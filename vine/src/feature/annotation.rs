use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Annotation<T> {
  pub child: T,
  pub ty: Ty,
}

impl<T> ParsePratt<T, Annotation<T>> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    left_prec: Precedence,
    initial: T,
  ) -> Result<Result<Annotation<T>, T>, Diag> {
    if left_prec.weaker(Precedence::Annotation) && self.eat(Token::Colon)? {
      let ty = self.parse_pratt(Precedence::Annotation)?;
      // todo
      Ok(Ok(Annotation { child: initial, ty }))
    } else {
      Ok(Err(initial))
    }
  }
}
