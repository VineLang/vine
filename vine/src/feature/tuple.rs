use crate::feature::placement::Placement;

use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Tuple<T> {
  pub children: Sequence<T>,
}


impl GrammarPratt for Tuple<Expr> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Expr, Tuple<Expr>> for Parser<'_> {} // todo

impl GrammarPratt for Tuple<Pat> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Pat, Tuple<Pat>> for Parser<'_> {} // todo

impl GrammarPratt for Tuple<Ty> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Ty, Tuple<Ty>> for Parser<'_> {} // todo

trait ParseParens<T, K> {
  fn from_paren(term: T) -> K;
  fn from_tuple(sequence: Sequence<T>) -> K;
  const ALLOW_PLACEMENT: bool;
  fn from_placement(placement: Placement<T>) -> K;
}
