use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Hole;

impl<T> ParsePratt<T, Hole> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Hole>, Diag> {
    if self.eat(Token::QuestQuest)? { Ok(Some(Hole)) } else { Ok(None) }
  }
}
