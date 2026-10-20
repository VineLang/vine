use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprTry {
  pub result: Expr,
  pub try_: Spanned<()>,
}

impl GrammarPratt for ExprTry {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![(
      Prec::Postfix,
      grammar.named("Try", (left::<Expr>(Prec::Postfix), Token::Dot, Token::Try)),
    )]
  }
}

impl ParsePratt<Expr, ExprTry> for Parser<'_> {
  fn _parse_dot(&mut self, initial: Expr) -> Result<Result<ExprTry, Expr>, Diag> {
    if let Some(try_) = self.eat_spanned(Token::Try)? {
      Ok(Ok(ExprTry { result: initial, try_ }))
    } else {
      Ok(Err(initial))
    }
  }
}
