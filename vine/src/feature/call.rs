use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprCall {
  pub callee: Expr,
  pub args: Sequence<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TraitFn {
  pub callee: Ty,
  pub args: Sequence<Ty>,
  pub return_: Option<Ty>,
}

impl GrammarPrattKind for ExprCall {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Postfix {
      rules.push(grammar.named(
        "Call",
        (left::<Expr>(prec).field("callee"), Token::Dot, rule::<Sequence<Expr>>().field("args")),
      ))
    }
  }
}

impl ParsePratt<Expr, ExprCall> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    _: &mut PrecCtx,
    initial: Expr,
  ) -> Result<Result<ExprCall, Expr>, Diag> {
    if let Some(args) = self.try_parse::<Sequence<Expr>>()? {
      Ok(Ok(ExprCall { callee: initial, args }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl GrammarPrattKind for TraitFn {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Max {
      rules.push(grammar.named(
        "Fn",
        (
          Token::Fn,
          left::<Ty>(Prec::Postfix).field("callee"),
          rule::<Sequence<Ty>>().field("args"),
          (Token::Colon, rule::<Ty>()).field("return").optional(),
        ),
      ))
    }
  }
}

impl ParsePratt<Trait, TraitFn> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<TraitFn>, Diag> {
    if self.eat(Token::Fn)? {
      let callee = self.parse()?;
      let args = self.parse()?;
      let return_ = self.eat_then(Token::Type, Self::parse)?;
      Ok(Some(TraitFn { callee, args, return_ }))
    } else {
      Ok(None)
    }
  }
}
