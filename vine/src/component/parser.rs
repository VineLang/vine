use std::cmp::Ordering;

use super::*;

use vine_util::{
  lexer::Lex,
  parser::{Parse as ParserTrait, ParserState},
};

use crate::{
  component::lexer::{Diag, Lexer, Token},
  feature::{
    field::{Field, FieldKind},
    method::ExprMethod,
    string::StringLiteral,
  },
};

pub struct Parser<'src> {
  state: ParserState<'src, Lexer<'src>>,
  span_base: usize,
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
    Ok(Parser { state: ParserState::new(lexer)?, span_base: 0 })
  }

  pub fn anchor_spans<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> (FileSpan, T) {
    let old_base = self.span_base;
    self.span_base = self.state.lexer.offset();
    let result = f(self);
    let file_span = FileSpan { start: self.span_base, end: self.state.lexer.offset() };
    self.span_base = old_base;
    (file_span, result)
  }

  pub fn start_span(&self) -> usize {
    self.state.lexer.offset() - self.span_base
  }

  pub fn end_span(&self, span: usize) -> LocalSpan {
    LocalSpan { start: span, end: self.state.last_token_end - self.span_base }
  }

  pub fn token_span(&self) -> Spanned<()> {
    todo!()
  }

  pub fn eat_spanned(&mut self, token: Token) -> Result<Option<Spanned<()>>, Diag> {
    let span = self.token_span();
    Ok(self.eat(token)?.then_some(span))
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

  pub fn parse_pratt<T>(&mut self, left_prec: Prec) -> Result<T, Diag>
  where
    Self: ParsePratt<T, T>,
  {
    self.try_parse_pratt(left_prec)?.ok_or_else(|| self.unexpected_error())
  }

  pub fn try_parse_pratt<T>(&mut self, left_prec: Prec) -> Result<Option<T>, Diag>
  where
    Self: ParsePratt<T, T>,
  {
    let ctx = &mut PrecCtx { outer: left_prec, inner: Prec::Max };

    let Some(mut term) = self._parse_initial(ctx)? else { return Ok(None) };

    loop {
      if Self::ALLOW_DOT && self.eat(Token::Dot)? {
        return match self._parse_dot(term)? {
          Ok(term) => Ok(Some(term)),
          Err(_) => self.unexpected(),
        };
      }
      match self._parse_suffix(ctx, term)? {
        Ok(term_) => {
          term = term_;
          continue;
        }
        Err(term) => return Ok(Some(term)),
      }
    }
  }

  pub fn semi(&mut self, mandatory: bool) -> Result<(), Diag> {
    todo!() // todo
  }
}

impl Parse<Ident> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Ident>, Diag> {
    if self.eat(Token::Backslash)? {
      if let Some(string) = self.try_parse::<StringLiteral>()? {
        Ok(Some(Ident(string.value())))
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

#[allow(unused_variables)]
pub trait ParsePratt<T, K = T> {
  fn _parse_full(&mut self) -> Result<Option<K>, Diag> {
    Ok(None)
  }

  fn _parse_initial(&mut self, ctx: &mut PrecCtx) -> Result<Option<K>, Diag> {
    Ok(None)
  }

  fn _parse_suffix(&mut self, ctx: &mut PrecCtx, initial: T) -> Result<Result<K, T>, Diag> {
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
    if let Some(term) = self._parse_full()? {
      return Ok(Some(term));
    }
    self.try_parse_pratt(Prec::Min)
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

      fn _parse_initial(&mut self, ctx: &mut PrecCtx) -> Result<Option<$Term>, Diag> {
        let span = self.start_span();
        if let Some(kind) = ParsePratt::<$Term, $TermKind>::_parse_initial(self, ctx)? {
          return Ok(Some($Term::new(self.end_span(span), kind)))
        }
        for_enum!(Kind in $TermKind { $_(
          if let Some(kind) = ParsePratt::<$Term, _>::_parse_initial(self, ctx)? {
            return Ok(Some($Term::new(self.end_span(span), $TermKind::$Kind(kind))))
          }
        )* });
        Ok(None)
      }

      fn _parse_suffix(
        &mut self,
        ctx: &mut PrecCtx,
        mut initial: $Term,
      ) -> Result<Result<$Term, $Term>, Diag> {
        let span = initial.span.start;
        match ParsePratt::<$Term, $TermKind>::_parse_suffix(self, ctx, initial)? {
          Ok(kind) => return Ok(Ok($Term::new(self.end_span(span), kind))),
          Err(initial_) => initial = initial_,
        }
        for_enum!(Kind in $TermKind { $_(
          match self._parse_suffix(ctx, initial)? {
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

  fn _parse_dot(&mut self, initial: Expr) -> Result<Result<ExprKind, Expr>, Diag> {
    if let Some(ident) = self.try_parse()? {
      if self.check(Token::OpenBracket) || self.check(Token::OpenParen) {
        let generics = self.parse()?;
        let args = self.parse()?;
        return Ok(Ok(ExprKind::Method(ExprMethod {
          recv: initial,
          method: ident,
          generics,
          args,
        })));
      } else {
        return Ok(Ok(ExprKind::Field(Field {
          composite: initial,
          field: FieldKind::Object(ident),
        })));
      }
    }
    Ok(Err(initial))
  }
}

impl ParsePratt<Pat, PatKind> for Parser<'_> {}

impl ParsePratt<Ty, TyKind> for Parser<'_> {
  const ALLOW_DOT: bool = true;
}

impl ParsePratt<Impl, ImplKind> for Parser<'_> {}

impl ParsePratt<Trait, TraitKind> for Parser<'_> {}

pub trait ParseAttr<T> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<T>, Diag>;
}

pub struct PrecCtx {
  outer: Prec,
  inner: Prec,
}

impl PrecCtx {
  pub fn prec(&mut self, prec: Prec) -> Option<impl FnOnce() -> Prec> {
    if Assoc::between(self.outer, prec) == Assoc::Right
      && Assoc::between(self.inner, prec) == Assoc::Left
    {
      Some(move || {
        self.inner = prec;
        prec
      })
    } else {
      None
    }
  }
}

#[derive_(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
  Min,
  Cond,
  LogicalImplies,
  LogicalOr,
  LogicalAnd,
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
  Max,
}

#[derive_(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
  None,
  Left,
  Right,
}

impl Assoc {
  pub fn between(left_prec: Prec, right_prec: Prec) -> Assoc {
    match left_prec.cmp(&right_prec) {
      Ordering::Less => Assoc::Right,
      Ordering::Equal => left_prec.assoc(),
      Ordering::Greater => Assoc::Left,
    }
  }
}

impl Prec {
  pub fn assoc(self) -> Assoc {
    use Assoc::*;
    match self {
      Prec::Min => None,
      Prec::Cond => Right,
      Prec::LogicalImplies => Right,
      Prec::LogicalOr => None,
      Prec::LogicalAnd => None,
      Prec::Comparison => None,
      Prec::Range => None,
      Prec::BitOr => Left,
      Prec::BitXor => Left,
      Prec::BitAnd => Left,
      Prec::BitShift => Left,
      Prec::Additive => Left,
      Prec::Multiplicative => Left,
      Prec::Exponential => Right,
      Prec::Annotation => None,
      Prec::Prefix => Right,
      Prec::Postfix => Left,
      Prec::Max => None,
    }
  }
}
