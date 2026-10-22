use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprIf {
  pub label: Label,
  pub cond: IfCond,
  pub then: Block,
  pub else_: Option<Block>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum IfCond {
  Const(Path<!>),
  Expr(Expr),
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyIfConst {
  pub cond: Path<!>,
  pub then: Ty,
  pub else_: Option<Ty>,
}

impl GrammarRule for ExprIf {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "ExprIf",
      (
        Token::If,
        rule::<Label>().optional(),
        below::<Expr>(Prec::Block),
        rule::<Block>().field("body"),
        (Token::Else, rule::<Block>().field("else")).optional(),
      ),
    )
  }
}

impl Parse<ExprIf> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<ExprIf>, Diag> {
    if self.eat(Token::If)? {
      let label = self.parse()?;
      let cond = if self.eat(Token::Const)? {
        IfCond::Const(self.parse()?)
      } else {
        IfCond::Expr(self.parse_pratt(Prec::Block)?)
      };
      let then = self.parse()?;
      let else_ = self.eat_then(Token::Else, Self::parse)?;
      Ok(Some(ExprIf { label, cond, then, else_ }))
    } else {
      Ok(None)
    }
  }
}

impl GrammarPrattKind for TyIfConst {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*; // todo
  }
}

impl ParsePratt<Ty, TyIfConst> for Parser<'_> {
  fn _parse_initial(&mut self, ctx: &mut PrecCtx) -> Result<Option<TyIfConst>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Min)
      && self.eat(Token::If)?
    {
      _ = prec();
      let cond = self.parse()?;
      self.expect(Token::OpenBrace)?;
      let then = self.parse()?;
      self.expect(Token::CloseBrace)?;
      let else_ = self.eat_then(Token::Else, |self_| {
        self_.expect(Token::OpenBrace)?;
        let ty = self_.parse()?;
        self_.expect(Token::CloseBrace)?;
        Ok(ty)
      })?;
      Ok(Some(TyIfConst { cond, then, else_ }))
    } else {
      Ok(None)
    }
  }
}
