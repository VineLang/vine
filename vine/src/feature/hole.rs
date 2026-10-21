use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Hole;

impl GrammarPrattKind for Hole {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    if prec == Prec::Max {
      rules.push(grammar.named("Hole", Token::QuestQuest));
    }
  }
}

impl<T> ParsePratt<T, Hole> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Hole>, Diag> {
    if self.eat(Token::QuestQuest)? { Ok(Some(Hole)) } else { Ok(None) }
  }
}
