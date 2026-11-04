use super::*;

use crate::feature::children::ChildrenEntry;

#[derive_(Debug, Clone, Visit!)]
pub struct ExprIs {
  pub expr: Expr,
  pub pat: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprImplies {
  pub asusmption: Expr,
  pub conclusion: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprAll {
  pub children: Children<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprAny {
  pub children: Children<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprNot {
  pub child: Expr,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatIs {
  pub bind: Pat,
  pub pat: Pat,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatAny {
  pub children: Children<Pat>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatCond {
  pub pat: Pat,
  pub cond: Expr,
}

trait AnyAll: Sized {
  type Child;
  const GROUP: Token;
  const INFIX: Token;
  const PREC: Prec;
  const EMPTY: Option<Token>;
  fn new(children: Children<Self::Child>) -> Self;
}

impl AnyAll for ExprAny {
  type Child = Expr;
  const GROUP: Token = Token::Any;
  const INFIX: Token = Token::Or;
  const PREC: Prec = Prec::LogicalOr;
  const EMPTY: Option<Token> = Some(Token::False);
  fn new(children: Children<Self::Child>) -> Self {
    ExprAny { children }
  }
}

impl AnyAll for ExprAll {
  type Child = Expr;
  const GROUP: Token = Token::All;
  const INFIX: Token = Token::And;
  const PREC: Prec = Prec::LogicalAnd;
  const EMPTY: Option<Token> = Some(Token::True);
  fn new(children: Children<Self::Child>) -> Self {
    ExprAll { children }
  }
}

impl AnyAll for PatAny {
  type Child = Pat;
  const GROUP: Token = Token::Any;
  const INFIX: Token = Token::Or;
  const PREC: Prec = Prec::LogicalOr;
  const EMPTY: Option<Token> = None;
  fn new(children: Children<Self::Child>) -> Self {
    PatAny { children }
  }
}

impl GrammarPrattKind for ExprIs {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Comparison {
      rules.push(grammar.named("Is", (left::<Expr>(prec), Token::Is, right::<Pat>(prec))));
    }
  }
}

impl GrammarPrattKind for ExprImplies {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::LogicalImplies {
      rules.push(grammar.named("Implies", (left::<Expr>(prec), Token::Impl, right::<Pat>(prec))));
    }
  }
}

impl GrammarPrattKind for ExprNot {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Prefix {
      rules.push(grammar.named("Not", (Token::Bang, right::<Expr>(prec))));
    }
  }
}

impl GrammarPrattKind for PatIs {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Comparison {
      rules.push(grammar.named("Is", (left::<Pat>(prec), Token::Is, right::<Pat>(prec))));
    }
  }
}

impl GrammarPrattKind for PatCond {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Cond {
      rules.push(grammar.named("If", (left::<Pat>(prec), Token::If, right::<Expr>(prec))));
    }
  }
}

impl<T: GrammarPratt, A: AnyAll<Child = T>> GrammarPrattKind for A {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Max {
      if let Some(token) = A::EMPTY {
        rules.push(grammar.named(format!("{:?}", token), token));
      }
      rules.push(grammar.named(
        format!("{:?}", A::GROUP),
        (A::GROUP, delimited::<T>(Token::OpenBrace, Token::Comma, Token::CloseBrace)),
      ));
    }
    if prec == A::PREC {
      rules.push(grammar.named(
        format!("{:?}", A::INFIX),
        (below::<T>(prec), (A::INFIX, below::<T>(prec)).repeat(true)),
      ));
    }
  }
}

impl ParsePratt<Expr, ExprIs> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: Expr,
  ) -> Result<Result<ExprIs, Expr>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Comparison)
      && self.eat(Token::Is)?
    {
      let pat = self.parse_pratt(prec())?;
      Ok(Ok(ExprIs { expr: initial, pat }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl ParsePratt<Expr, ExprImplies> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: Expr,
  ) -> Result<Result<ExprImplies, Expr>, Diag> {
    if let Some(prec) = ctx.prec(Prec::LogicalImplies)
      && self.eat(Token::Impl)?
    {
      let conclusion = self.parse_pratt(prec())?;
      Ok(Ok(ExprImplies { asusmption: initial, conclusion }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl ParsePratt<Expr, ExprNot> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<ExprNot>, Diag> {
    if self.eat(Token::Bang)? {
      let child = self.parse_pratt(Prec::Prefix)?;
      Ok(Some(ExprNot { child }))
    } else {
      Ok(None)
    }
  }
}

impl ParsePratt<Pat, PatIs> for Parser<'_> {
  fn _parse_suffix(&mut self, ctx: &mut PrecCtx, initial: Pat) -> Result<Result<PatIs, Pat>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Comparison)
      && self.eat(Token::Is)?
    {
      let pat = self.parse_pratt(prec())?;
      Ok(Ok(PatIs { bind: initial, pat }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl ParsePratt<Pat, PatCond> for Parser<'_> {
  fn _parse_suffix(
    &mut self,
    ctx: &mut PrecCtx,
    initial: Pat,
  ) -> Result<Result<PatCond, Pat>, Diag> {
    if let Some(prec) = ctx.prec(Prec::Cond)
      && self.eat(Token::If)?
    {
      let cond = self.parse_pratt(prec())?;
      Ok(Ok(PatCond { pat: initial, cond }))
    } else {
      Ok(Err(initial))
    }
  }
}

impl<T, A: AnyAll<Child = T>> ParsePratt<T, A> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<A>, Diag> {
    if let Some(token) = A::EMPTY
      && self.eat(token)?
    {
      Ok(Some(A::new(Default::default())))
    } else if self.eat(A::GROUP)? {
      let children =
        self.parse_children(Some(Token::OpenBrace), Some(Token::Comma), Some(Token::CloseBrace))?;
      Ok(Some(A::new(children)))
    } else {
      Ok(None)
    }
  }

  fn _parse_suffix(&mut self, ctx: &mut PrecCtx, initial: T) -> Result<Result<A, T>, Diag> {
    if let Some(prec) = ctx.prec(A::PREC)
      && self.eat(A::INFIX)?
    {
      _ = prec();
      let mut children = Children::default();
      children.entries.push(ChildrenEntry::from(initial));
      while self.eat(A::INFIX)? {
        children.entries.push(ChildrenEntry::from(self.parse_pratt(A::PREC)?));
      }
      Ok(Ok(A::new(children)))
    } else {
      Ok(Err(initial))
    }
  }
}
