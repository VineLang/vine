use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Placement<T> {
  pub value: T,
  pub space: T,
}

impl<T: GrammarRule> GrammarPratt for Placement<T> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![(
      Prec::Max,
      grammar.named(
        "Placement",
        (Token::OpenParen, rule::<T>(), Token::Semi, rule::<T>(), Token::CloseParen),
      ),
    )]
  }
}

impl ParsePratt<Expr, Placement<Expr>> for Parser<'_> {} // todo

impl ParsePratt<Pat, Placement<Pat>> for Parser<'_> {} // todo
