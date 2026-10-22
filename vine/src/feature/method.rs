use super::*;

use crate::feature::generics::GenericArgs;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprMethod {
  pub recv: Expr,
  pub method: Spanned<Ident>,
  pub generics: GenericArgs,
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
              rule::<Ident>().field("name"),
              rule::<GenericArgs>().field("generics").optional(),
              rule::<Sequence<Expr>>().field("args"),
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
