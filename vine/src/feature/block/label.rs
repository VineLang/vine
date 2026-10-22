use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct StmtBreak {
  pub target: Target,
  pub value: Option<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct StmtContinue {
  pub target: Target,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Label {
  pub kind: LabelKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum LabelKind {
  None,
  Named(Spanned<Ident>),
}

#[derive_(Debug, Clone, Visit!)]
pub struct Target {
  pub kind: TargetKind,
}

#[derive_(Debug, Clone, Visit!)]
pub enum TargetKind {
  None,
  Named(Spanned<Ident>),
  Keyword(TargetKeyword),
}

#[derive_(Debug, Clone, Visit!, for_enum!)]
pub enum TargetKeyword {
  Loop,
  For,
  While,
  Do,
  If,
  Match,
  When,
  Fn,
}

impl GrammarRule for StmtBreak {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      "StmtBreak",
      (
        choice(((Token::Break, rule::<Target>().optional()), Token::Return)),
        rule::<Expr>().optional(),
        Token::Semi,
      ),
    )
  }
}

impl GrammarRule for StmtContinue {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named("StmtContinue", (Token::Continue, rule::<Target>().optional(), Token::Semi))
  }
}

impl GrammarRule for Label {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named("Label", (Token::Colon, rule::<Ident>()))
  }
}

#[rustfmt::skip] // if omitted, rustfmt will fail to format the whole file
impl GrammarRule for Target {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named("Target", (
      Token::Colon,
      choice(for_enum!(Keyword in TargetKeyword { (
        rule::<Ident>(), 
        $(Token::$Keyword,)*
      ) })),
    ))
  }
}

impl Parse<StmtBreak> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtBreak>, Diag> {
    if self.eat(Token::Break)? {
      let target = self.parse()?;
      let value = self.try_parse()?;
      Ok(Some(StmtBreak { target, value }))
    } else if self.eat(Token::Return)? {
      let target = Target { kind: TargetKind::Keyword(TargetKeyword::Fn) };
      let value = self.try_parse()?;
      Ok(Some(StmtBreak { target, value }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<StmtContinue> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<StmtContinue>, Diag> {
    if self.eat(Token::Continue)? {
      let target = self.parse()?;
      Ok(Some(StmtContinue { target }))
    } else {
      Ok(None)
    }
  }
}

impl Parse<Label> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Label>, Diag> {
    if self.eat(Token::Colon)? {
      Ok(Some(Label { kind: LabelKind::Named(self.parse()?) }))
    } else {
      Ok(Some(Label { kind: LabelKind::None }))
    }
  }
}

impl Parse<Target> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Target>, Diag> {
    if self.eat(Token::Colon)? {
      if let Some(ident) = self.try_parse()? {
        Ok(Some(Target { kind: TargetKind::Named(ident) }))
      } else {
        Ok(Some(Target { kind: TargetKind::Keyword(self.parse()?) }))
      }
    } else {
      Ok(Some(Target { kind: TargetKind::None }))
    }
  }
}

impl Parse<TargetKeyword> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<TargetKeyword>, Diag> {
    for_enum!(Keyword in TargetKeyword { $(
      if self.eat(Token::$Keyword)? {
        return Ok(Some(TargetKeyword::$Keyword));
      }
    )* });
    Ok(None)
  }
}
