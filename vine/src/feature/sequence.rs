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

pub trait GrammarSequence {
  const ALLOW_LABEL: bool = false;
  const NAME: &str;
}

impl<T: GrammarRule + GrammarSequence> GrammarRule for Sequence<T> {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      format!("{}Sequence", T::NAME),
      (
        Token::OpenParen,
        (rule::<T>(), (Token::Comma, rule::<T>()).repeat(false), Token::Comma.optional())
          .optional(),
        Token::CloseParen,
      ),
    )
  }
}

impl<T: GrammarRule + GrammarSequence> GrammarRule for SequenceChild<T> {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    let label = if T::ALLOW_LABEL {
      (rule::<Ident>(), Token::Colon).optional().to_node(grammar)
    } else {
      empty().to_node(grammar)
    };
    grammar.named(
      format!("{}SequenceChild", T::NAME),
      choice(((label, rule::<T>()), (Token::DotDotDot, rule::<T>().optional()))),
    )
  }
}

impl GrammarSequence for Expr {
  const NAME: &str = "Expr";
}

impl GrammarSequence for Pat {
  const NAME: &str = "Pat";
}

impl GrammarSequence for Ty {
  const NAME: &str = "Ty";
  const ALLOW_LABEL: bool = true;
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
