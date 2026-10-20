use super::*;

/// A tuple-like collection of children, with regular entries and spreads.
pub type Sequence<T> = Children<SequenceChild<T>>;

#[derive_(Debug, Clone, Visit!)]
pub enum SequenceChild<T> {
  Entry {
    /// Can be present in tuple types; serves solely as documentation.
    label: Option<Spanned<Ident>>,
    child: T,
  },
  Spread(Option<T>),
}

pub trait ParseSequence: Sized {
  fn as_label(&self) -> Option<Spanned<Ident>> {
    None
  }
}

impl<T: ParseSequence> Parse<Sequence<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse(&mut self) -> Result<Option<Sequence<T>>, Diag> {
    self.eat_then(Token::OpenParen, |self_| {
      self_.parse_children::<SequenceChild<T>>(None, None, Some(Token::CloseParen))
    })
  }
}

impl<T: ParseSequence> Parse<SequenceChild<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse(&mut self) -> Result<Option<SequenceChild<T>>, Diag> {
    if let Some(child) = self.try_parse()? {
      if let Some(label) = child.as_label()
        && self.eat(Token::Colon)?
      {
        let child = self.parse()?;
        Ok(Some(SequenceChild::Entry { label: Some(label.clone()), child }))
      } else {
        Ok(Some(SequenceChild::Entry { label: None, child }))
      }
    } else if self.eat(Token::DotDotDot)? {
      let spread = self.try_parse()?;
      Ok(Some(SequenceChild::Spread(spread)))
    } else {
      Ok(None)
    }
  }
}

impl ParseSequence for Expr {}
impl ParseSequence for Pat {}
impl ParseSequence for Ty {
  fn as_label(&self) -> Option<Spanned<Ident>> {
    match &*self.kind {
      TyKind::Path(path) => path.as_ident(),
      _ => None,
    }
  }
}
