use crate::feature::{
  placement::Placement,
  sequence::{SequenceChild, SequenceKind},
};

use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct Tuple<T> {
  pub children: Sequence<T>,
}

impl<T: SequenceKind + GrammarPratt> GrammarPrattKind for Tuple<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Max {
      rules.push(
        grammar.named(
          "Tuple",
          (
            Token::OpenParen,
            (
              rule::<SequenceChild<T>>(),
              Token::Comma,
              (rule::<SequenceChild<T>>(), Token::Comma).repeat(false),
              rule::<SequenceChild<T>>().optional(),
            )
              .optional(),
            Token::CloseParen,
          ),
        ),
      )
    }
  }
}

impl ParsePratt<Expr, Tuple<Expr>> for Parser<'_> {} // todo
impl ParsePratt<Pat, Tuple<Pat>> for Parser<'_> {} // todo
impl ParsePratt<Ty, Tuple<Ty>> for Parser<'_> {} // todo

trait ParseParens<T, K> {
  fn from_paren(term: T) -> K;
  fn from_tuple(sequence: Sequence<T>) -> K;
  const ALLOW_PLACEMENT: bool;
  fn from_placement(placement: Placement<T>) -> K;
}
