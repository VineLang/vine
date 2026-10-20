use super::*;

use crate::feature::generics::GenericParams;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprMethod {
  pub recv: Expr,
  pub method: Spanned<Ident>,
  pub generics: GenericParams,
  pub args: Sequence<Expr>,
}

impl GrammarPratt for ExprMethod {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![(
      Prec::Postfix,
      grammar.named(
        "Method",
        (
          left::<Expr>(Prec::Postfix),
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
    )]
  }
}

// `_parse_dot` handled by `ParsePratt<Expr, ExprKind>`
impl ParsePratt<Expr, ExprMethod> for Parser<'_> {}
