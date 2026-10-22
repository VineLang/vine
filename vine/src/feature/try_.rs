use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprTry {
  pub result: Expr,
  pub try_: Spanned<()>,
}

impl GrammarPrattKind for ExprTry {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Postfix {
      rules.push(grammar.named("Try", (left::<Expr>(prec), Token::Dot, Token::Try)));
    }
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
