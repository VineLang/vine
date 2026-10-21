use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Placement<T> {
  pub value: T,
  pub space: T,
}

impl<T: GrammarRule> GrammarPrattKind for Placement<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Max {
      rules.push(grammar.named(
        "Placement",
        (Token::OpenParen, rule::<T>(), Token::Semi, rule::<T>(), Token::CloseParen),
      ));
    }
  }
}

impl ParsePratt<Expr, Placement<Expr>> for Parser<'_> {} // todo

impl ParsePratt<Pat, Placement<Pat>> for Parser<'_> {} // todo
