use std::cmp::Ordering;

use super::*;

use vine_util::{
  lexer::Lex,
  parser::{Parse as ParserTrait, ParserState},
};

use crate::component::lexer::{Diag, Lexer, Token};

pub struct Parser<'src> {
  state: ParserState<'src, Lexer<'src>>,
}

impl<'src> ParserTrait<'src> for Parser<'src> {
  type Token = Token;
  type Lexer = Lexer<'src>;
  type Error = Diag;

  fn state(&mut self) -> &mut ParserState<'src, Lexer<'src>> {
    &mut self.state
  }

  fn unexpected_error(&self) -> Diag {
    todo!()
    // Diag::UnexpectedToken {
    //   span: self.span(),
    //   expected: self.state.expected.clone(),
    //   found: self.state.token,
    // }
  }
}

impl<'src> Parser<'src> {
  pub fn new(lexer: Lexer<'src>) -> Result<Self, Diag> {
    Ok(Parser { state: ParserState::new(lexer)? })
  }

  pub fn start_span(&self) -> usize {
    todo!()
  }

  pub fn end_span(&self, span: usize) -> Span {
    _ = span;
    todo!()
  }

  pub fn token_span(&self) -> Spanned<()> {
    todo!()
  }

  pub fn spanned<T>(
    &mut self,
    f: impl FnOnce(&mut Self) -> Result<T, Diag>,
  ) -> Result<Spanned<T>, Diag> {
    let span = self.start_span();
    let inner = f(self)?;
    let span = self.end_span(span);
    Ok(Spanned { span, inner })
  }

  pub fn parse<T>(&mut self) -> Result<T, Diag>
  where
    Self: Parse<T>,
  {
    self.try_parse()?.ok_or_else(|| self.unexpected_error())
  }

  pub fn try_parse<T>(&mut self) -> Result<Option<T>, Diag>
  where
    Self: Parse<T>,
  {
    self._parse()
  }

  pub fn parse_pratt<T>(&mut self, left_prec: Precedence) -> Result<T, Diag>
  where
    Self: ParsePratt<T, T>,
  {
    self.try_parse_pratt(left_prec)?.ok_or_else(|| self.unexpected_error())
  }

  pub fn try_parse_pratt<T>(&mut self, left_prec: Precedence) -> Result<Option<T>, Diag>
  where
    Self: ParsePratt<T, T>,
  {
    if left_prec == Precedence::Root
      && let Some(term) = self._parse_full()?
    {
      return Ok(Some(term));
    }

    let Some(mut term) = self._parse_initial(left_prec)? else { return Ok(None) };

    loop {
      if Self::ALLOW_DOT && self.eat(Token::Dot)? {
        return match self._parse_dot(term)? {
          Ok(term) => Ok(Some(term)),
          Err(_) => self.unexpected(),
        };
      }
      match self._parse_suffix(left_prec, term)? {
        Ok(term_) => {
          term = term_;
          continue;
        }
        Err(term) => return Ok(Some(term)),
      }
    }
  }

  pub fn parse_children<T>(
    &mut self,
    open: Option<Token>,
    separator: Option<Token>,
    close: Option<Token>,
  ) -> Result<Children<T>, Diag>
  where
    Self: Parse<T>,
  {
    assert!(separator.is_some() || close.is_some());
    if let Some(open) = open {
      self.expect(open)?;
    }
    let mut entries = Vec::new();
    loop {
      if let Some(close) = close
        && self.check(close)
      {
        break;
      }
      let docs = Text { lines: vec![] }; // todo
      let trailing = Text { lines: vec![] };
      entries.push(ChildrenEntry::Child { child: self.parse()?, docs, trailing });
      if let Some(separator) = separator
        && !self.eat(separator)?
      {
        break;
      }
    }
    if let Some(close) = close {
      self.expect(close)?;
    }
    Ok(Children { entries })
  }
}

impl Parse<Ident> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Ident>, Diag> {
    if self.eat(Token::Backslash)? {
      if self.check(Token::OpenString) {
        todo!()
      } else {
        self.state.expected.add(Token::GroupKeyword);
        if !self.state.token.is_keyword() {
          self.unexpected()?;
        }
        let str = self.state.lexer.slice();
        self.bump()?;
        Ok(Some(Ident(str.into())))
      }
    } else if self.check(Token::BareIdent) {
      Ok(Some(Ident(self.expect(Token::BareIdent)?.into())))
    } else {
      Ok(None)
    }
  }
}

pub trait ParseSequence: Sized {
  fn as_label(&self) -> Option<&Spanned<Ident>> {
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
  fn as_label(&self) -> Option<&Spanned<Ident>> {
    todo!()
  }
}

pub trait ParseObject: Sized {
  const ALLOW_SHORTHAND: bool;
  fn from_shorthand(ident: Spanned<Ident>, annotation: Option<Ty>) -> Self;
}

impl<T: ParseObject> Parse<Object<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse(&mut self) -> Result<Option<Object<T>>, Diag> {
    if self.eat(Token::OpenBrace)? {
      let ordered = self.eat(Token::OpenBrace)?;
      let children =
        self.parse_children::<ObjectChild<T>>(None, Some(Token::Comma), Some(Token::CloseBrace))?;
      if ordered {
        self.expect(Token::CloseBrace)?;
      }
      Ok(Some(Object { ordered, children }))
    } else {
      Ok(None)
    }
  }
}

impl<T: ParseObject> Parse<ObjectChild<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse(&mut self) -> Result<Option<ObjectChild<T>>, Diag> {
    if let Some(key) = self.try_parse::<Spanned<Ident>>()? {
      if self.eat(Token::Colon)? {
        if let Some(value) = self.try_parse()? {
          Ok(Some(ObjectChild::Entry { key, value }))
        } else if T::ALLOW_SHORTHAND && self.eat(Token::Colon)? {
          let ty = self.parse_pratt(Precedence::Annotation)?;
          let value = T::from_shorthand(key.clone(), Some(ty));
          Ok(Some(ObjectChild::Entry { key, value }))
        } else {
          self.unexpected()
        }
      } else if T::ALLOW_SHORTHAND {
        let value = T::from_shorthand(key.clone(), None);
        Ok(Some(ObjectChild::Entry { key, value }))
      } else {
        self.unexpected()
      }
    } else if self.eat(Token::DotDotDot)? {
      let spread = self.try_parse()?;
      Ok(Some(ObjectChild::Spread(spread)))
    } else {
      Ok(None)
    }
  }
}

impl<T> Parse<Spanned<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse(&mut self) -> Result<Option<Spanned<T>>, Diag> {
    let span = self.start_span();
    if let Some(inner) = self.try_parse()? {
      let span = self.end_span(span);
      Ok(Some(Spanned { span, inner }))
    } else {
      Ok(None)
    }
  }
}

pub trait Parse<T> {
  fn _parse(&mut self) -> Result<Option<T>, Diag>;
}

pub trait ParsePratt<T, K = T> {
  fn _parse_full(&mut self) -> Result<Option<K>, Diag> {
    Ok(None)
  }

  fn _parse_initial(&mut self, _left_prec: Precedence) -> Result<Option<K>, Diag> {
    Ok(None)
  }

  fn _parse_suffix(&mut self, _left_prec: Precedence, initial: T) -> Result<Result<K, T>, Diag> {
    Ok(Err(initial))
  }

  const ALLOW_DOT: bool = false;

  fn _parse_dot(&mut self, initial: T) -> Result<Result<K, T>, Diag> {
    Ok(Err(initial))
  }
}

impl<T> Parse<T> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse(&mut self) -> Result<Option<T>, Diag> {
    self.try_parse_pratt(Precedence::Root)
  }
}

macro_rules! ParsePratt {
  (($_:tt) $($Term:ident $TermKind:ident),* $(,)?) => {
    $(impl ParsePratt<$Term> for Parser<'_> {
      fn _parse_full(&mut self) -> Result<Option<$Term>, Diag> {
        let span = self.start_span();
        if let Some(kind) = ParsePratt::<$Term, $TermKind>::_parse_full(self)? {
          return Ok(Some($Term::new(self.end_span(span), kind)))
        }
        for_enum!(Kind in $TermKind { $_(
          if let Some(kind) = ParsePratt::<$Term, _>::_parse_full(self)? {
            return Ok(Some($Term::new(self.end_span(span), $TermKind::$Kind(kind))))
          }
        )* });
        Ok(None)
      }

      fn _parse_initial(&mut self, left_prec: Precedence) -> Result<Option<$Term>, Diag> {
        let span = self.start_span();
        if let Some(kind) = ParsePratt::<$Term, $TermKind>::_parse_initial(self, left_prec)? {
          return Ok(Some($Term::new(self.end_span(span), kind)))
        }
        for_enum!(Kind in $TermKind { $_(
          if let Some(kind) = ParsePratt::<$Term, _>::_parse_initial(self, left_prec)? {
            return Ok(Some($Term::new(self.end_span(span), $TermKind::$Kind(kind))))
          }
        )* });
        Ok(None)
      }

      fn _parse_suffix(
        &mut self,
        left_prec: Precedence,
        mut initial: $Term,
      ) -> Result<Result<$Term, $Term>, Diag> {
        let span = initial.span.start;
        match ParsePratt::<$Term, $TermKind>::_parse_suffix(self, left_prec, initial)? {
          Ok(kind) => return Ok(Ok($Term::new(self.end_span(span), kind))),
          Err(initial_) => initial = initial_,
        }
        for_enum!(Kind in $TermKind { $_(
          match self._parse_suffix(left_prec, initial)? {
            Ok(kind) => return Ok(Ok($Term::new(self.end_span(span), $TermKind::$Kind(kind)))),
            Err(initial_) => initial = initial_,
          }
        )* });
        Ok(Err(initial))
      }

      const ALLOW_DOT: bool = <Self as ParsePratt<$Term, $TermKind>>::ALLOW_DOT;

      fn _parse_dot(&mut self, mut initial: $Term) -> Result<Result<$Term, $Term>, Diag> {
        let span = initial.span.start;
        match ParsePratt::<$Term, $TermKind>::_parse_dot(self, initial)? {
          Ok(kind) => return Ok(Ok($Term::new(self.end_span(span), kind))),
          Err(initial_) => initial = initial_,
        }
        for_enum!(Kind in $TermKind { $_(
          match self._parse_dot(initial)? {
            Ok(kind) => return Ok(Ok($Term::new(self.end_span(span), $TermKind::$Kind(kind)))),
            Err(initial_) => initial = initial_,
          }
        )* });
        Ok(Err(initial))
      }
    })*
  };
}

ParsePratt!(($) Expr ExprKind, Pat PatKind, Ty TyKind, Impl ImplKind, Trait TraitKind);

impl ParsePratt<Expr, ExprKind> for Parser<'_> {
  const ALLOW_DOT: bool = true;
}

impl ParsePratt<Pat, PatKind> for Parser<'_> {}

impl ParsePratt<Ty, TyKind> for Parser<'_> {
  const ALLOW_DOT: bool = true;
}

impl ParsePratt<Impl, ImplKind> for Parser<'_> {}

impl ParsePratt<Trait, TraitKind> for Parser<'_> {}

#[derive_(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
  Root,
  LogicalImplies,
  LogicalOr,
  LogicalAnd,
  Is,
  Comparison,
  Range,
  BitOr,
  BitXor,
  BitAnd,
  BitShift,
  Additive,
  Multiplicative,
  Exponential,
  Annotation,
  Prefix,
  Postfix,
}

#[derive_(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
  None,
  Left,
  Right,
}

impl Associativity {
  pub fn between(left_prec: Precedence, right_prec: Precedence) -> Associativity {
    match left_prec.cmp(&right_prec) {
      Ordering::Less => Associativity::Right,
      Ordering::Equal => left_prec.assoc(),
      Ordering::Greater => Associativity::Left,
    }
  }
}

impl Precedence {
  pub fn assoc(self) -> Associativity {
    use Associativity::*;
    use Precedence::*;
    match self {
      Root => None,
      Safe => Right,
      LogicalImplies => Right,
      LogicalOr => None,
      LogicalAnd => None,
      Is => None,
      Comparison => None,
      Range => None,
      BitOr => Left,
      BitXor => Left,
      BitAnd => Left,
      BitShift => Left,
      Additive => Left,
      Multiplicative => Left,
      Exponential => Right,
      Annotation => Right,
      Prefix => Right,
      Postfix => Left,
    }
  }

  pub fn weaker(self, right_prec: Precedence) -> bool {
    Associativity::between(self, right_prec) == Associativity::Right
  }
}
