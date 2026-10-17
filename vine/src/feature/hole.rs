use super::*;

pub type ExprHole = Hole;
pub type PatHole = Hole;
pub type TyHole = Hole;
pub type ImplHole = Hole;
pub type TraitHole = Hole;

#[derive_(Debug, Clone, Visit!)]
pub struct Hole;

impl<T> ParsePratt<T, Hole> for Parser<'_> {
  fn _parse_initial(&mut self, _: Precedence) -> Result<Option<Hole>, Diag> {
    if self.eat(Token::Underscore)? { Ok(Some(Hole)) } else { Ok(None) }
  }
}
