use std::cmp::Ordering;

use super::*;

use vine_util::parser::{Parse, ParserState};

use crate::component::lexer::{Diag, Lexer, Token};

pub struct Parser<'src> {
  state: ParserState<'src, Lexer<'src>>,
}

impl<'src> Parse<'src> for Parser<'src> {
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

  pub fn check_ident(&mut self) -> bool {
    self.check(Token::BareIdent) || self.check(Token::Backslash)
  }

  pub fn eat_ident(&mut self) -> Result<Option<Ident>, Diag> {
    self.check_ident().then(|| self.parse_ident()).transpose()
  }

  pub fn parse_ident(&mut self) -> Result<Ident, Diag> {
    todo!()
  }

  pub fn parse_expr(&mut self, left_prec: Precedence) -> Result<Expr, Diag> {
    self.pratt(left_prec)?.ok_or_else(|| self.unexpected_error())
  }

  pub fn parse_pat(&mut self, left_prec: Precedence) -> Result<Pat, Diag> {
    self.pratt(left_prec)?.ok_or_else(|| self.unexpected_error())
  }

  pub fn parse_ty(&mut self, left_prec: Precedence) -> Result<Ty, Diag> {
    self.pratt(left_prec)?.ok_or_else(|| self.unexpected_error())
  }

  pub fn parse_impl(&mut self, left_prec: Precedence) -> Result<Impl, Diag> {
    self.pratt(left_prec)?.ok_or_else(|| self.unexpected_error())
  }

  pub fn parse_trait(&mut self, left_prec: Precedence) -> Result<Trait, Diag> {
    self.pratt(left_prec)?.ok_or_else(|| self.unexpected_error())
  }

  fn pratt<T>(&mut self, left_prec: Precedence) -> Result<Option<T>, Diag>
  where
    Self: ParseKind<T, T>,
  {
    if left_prec == Precedence::Root
      && let Some(term) = self._parse_full()?
    {
      return Ok(Some(term));
    }

    let Some(mut term) = self._parse_initial()? else { return Ok(None) };

    loop {
      match self._parse_suffix(left_prec, term)? {
        Ok(term_) => {
          term = term_;
          continue;
        }
        Err(term) => return Ok(Some(term)),
      }
    }
  }
}

impl ParseKind<Expr, Expr> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<Expr>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: ExprKind| Expr::new(self.end_span(span), kind)))
  }

  fn _parse_initial(&mut self) -> Result<Option<Expr>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: ExprKind| Expr::new(self.end_span(span), kind)))
  }

  fn _parse_suffix(
    &mut self,
    left_prec: Precedence,
    initial: Expr,
  ) -> Result<Result<Expr, Expr>, Diag> {
    let span = initial.span.start;
    Ok(
      self
        ._parse_suffix(left_prec, initial)?
        .map(|kind: ExprKind| Expr::new(self.end_span(span), kind)),
    )
  }
}

impl ParseKind<Pat, Pat> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<Pat>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: PatKind| Pat::new(self.end_span(span), kind)))
  }

  fn _parse_initial(&mut self) -> Result<Option<Pat>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: PatKind| Pat::new(self.end_span(span), kind)))
  }

  fn _parse_suffix(
    &mut self,
    left_prec: Precedence,
    initial: Pat,
  ) -> Result<Result<Pat, Pat>, Diag> {
    let span = initial.span.start;
    Ok(
      self
        ._parse_suffix(left_prec, initial)?
        .map(|kind: PatKind| Pat::new(self.end_span(span), kind)),
    )
  }
}

impl ParseKind<Ty, Ty> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<Ty>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: TyKind| Ty::new(self.end_span(span), kind)))
  }

  fn _parse_initial(&mut self) -> Result<Option<Ty>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: TyKind| Ty::new(self.end_span(span), kind)))
  }

  fn _parse_suffix(&mut self, left_prec: Precedence, initial: Ty) -> Result<Result<Ty, Ty>, Diag> {
    let span = initial.span.start;
    Ok(
      self
        ._parse_suffix(left_prec, initial)?
        .map(|kind: TyKind| Ty::new(self.end_span(span), kind)),
    )
  }
}

impl ParseKind<Impl, Impl> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<Impl>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: ImplKind| Impl::new(self.end_span(span), kind)))
  }

  fn _parse_initial(&mut self) -> Result<Option<Impl>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: ImplKind| Impl::new(self.end_span(span), kind)))
  }

  fn _parse_suffix(
    &mut self,
    left_prec: Precedence,
    initial: Impl,
  ) -> Result<Result<Impl, Impl>, Diag> {
    let span = initial.span.start;
    Ok(
      self
        ._parse_suffix(left_prec, initial)?
        .map(|kind: ImplKind| Impl::new(self.end_span(span), kind)),
    )
  }
}

impl ParseKind<Trait, Trait> for Parser<'_> {
  fn _parse_full(&mut self) -> Result<Option<Trait>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: TraitKind| Trait::new(self.end_span(span), kind)))
  }

  fn _parse_initial(&mut self) -> Result<Option<Trait>, Diag> {
    let span = self.start_span();
    Ok(self._parse_initial()?.map(|kind: TraitKind| Trait::new(self.end_span(span), kind)))
  }

  fn _parse_suffix(
    &mut self,
    left_prec: Precedence,
    initial: Trait,
  ) -> Result<Result<Trait, Trait>, Diag> {
    let span = initial.span.start;
    Ok(
      self
        ._parse_suffix(left_prec, initial)?
        .map(|kind: TraitKind| Trait::new(self.end_span(span), kind)),
    )
  }
}

pub trait ParseKind<T, K> {
  fn _parse_full(&mut self) -> Result<Option<K>, Diag> {
    Ok(None)
  }

  fn _parse_initial(&mut self) -> Result<Option<K>, Diag> {
    Ok(None)
  }

  fn _parse_suffix(&mut self, _left_prec: Precedence, initial: T) -> Result<Result<K, T>, Diag> {
    Ok(Err(initial))
  }
}

macro_rules! ParseKind {
  (($_:tt) $($Term:ident $TermKind:ident),* $(,)?) => {
    $(impl ParseKind<$Term, $TermKind> for Parser<'_> {
      fn _parse_full(&mut self) -> Result<Option<$TermKind>, Diag> {
        for_enum!(Kind in $TermKind { $_(
          if let Some(kind) = self._parse_full()? {
            return Ok(Some($TermKind::$Kind(kind)))
          }
        )* });
        Ok(None)
      }

      fn _parse_initial(&mut self) -> Result<Option<$TermKind>, Diag> {
        for_enum!(Kind in $TermKind { $_(
          if let Some(kind) = self._parse_initial()? {
            return Ok(Some($TermKind::$Kind(kind)))
          }
        )* });
        Ok(None)
      }

      fn _parse_suffix(
        &mut self,
        left_prec: Precedence,
        mut initial: $Term,
      ) -> Result<Result<$TermKind, $Term>, Diag> {
        for_enum!(Kind in $TermKind { $_(
          match self._parse_suffix(left_prec, initial)? {
            Ok(kind) => return Ok(Ok($TermKind::$Kind(kind))),
            Err(initial_) => initial = initial_,
          }
        )* });
        Ok(Err(initial))
      }
    })*
  };
}

ParseKind!(($) Stmt StmtKind, Expr ExprKind, Pat PatKind, Ty TyKind, Impl ImplKind, Trait TraitKind);

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
