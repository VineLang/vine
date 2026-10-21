use super::*;

use crate::feature::generics::GenericParams;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprMethod {
  pub recv: Expr,
  pub method: Spanned<Ident>,
  pub generics: GenericParams,
  pub args: Sequence<Expr>,
}

impl GrammarPrattKind for ExprMethod {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Postfix {
      rules.push(
        grammar.named(
          "Method",
          (
            left::<Expr>(prec),
            (
              Token::Dot,
              rule::<Ident>(),
              rule::<GenericParams>().field("generics"),
              Token::OpenParen,
              // todo
            )
              .named("Method"),
          ),
        ),
      );
    }
  }
}

// `_parse_dot` handled by `ParsePratt<Expr, ExprKind>`
impl ParsePratt<Expr, ExprMethod> for Parser<'_> {}
