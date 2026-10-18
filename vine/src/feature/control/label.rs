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
