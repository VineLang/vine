use crate::feature::{annotation::Annotation, path::PathCall};

use super::*;

/// An object-like key-value collection, with key-value pairs and spreads.
#[derive_(Debug, Clone, Visit!)]
pub struct Object<T> {
  /// Whether the entries are specified in order.
  pub ordered: bool,
  pub children: Children<ObjectChild<T>>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ObjectChild<T> {
  Entry { key: Spanned<Ident>, value: T },
  Spread(Option<T>),
}

pub trait ObjectKind: Sized {
  const ALLOW_SHORTHAND: bool;
  fn from_shorthand(span: LocalSpan, ident: Spanned<Ident>, annotation: Option<Ty>) -> Self;
}

impl ObjectKind for Expr {
  const ALLOW_SHORTHAND: bool = true;
  fn from_shorthand(span: LocalSpan, ident: Spanned<Ident>, annotation: Option<Ty>) -> Self {
    let expr = Expr::new(ident.span, PathCall { path: Path::from(ident), args: None });
    match annotation {
      Some(ty) => Expr::new(span, Annotation { child: expr, ty }),
      None => expr,
    }
  }
}

impl ObjectKind for Pat {
  const ALLOW_SHORTHAND: bool = true;
  fn from_shorthand(span: LocalSpan, ident: Spanned<Ident>, annotation: Option<Ty>) -> Self {
    let pat = Pat::new(ident.span, PathCall { path: Path::from(ident), args: None });
    match annotation {
      Some(ty) => Pat::new(span, Annotation { child: pat, ty }),
      None => pat,
    }
  }
}

impl ObjectKind for Ty {
  const ALLOW_SHORTHAND: bool = false;
  fn from_shorthand(_: LocalSpan, _: Spanned<Ident>, _: Option<Ty>) -> Self {
    unreachable!()
  }
}

impl<T: ObjectKind + GrammarPratt> GrammarPrattKind for Object<T> {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<grammar::Rule>) {
    use grammar::*;
    if prec == Prec::Max {
      rules.push(grammar.named(
        "Object",
        choice((
          delimited::<ObjectChild<T>>(Token::OpenBrace, Token::Comma, Token::CloseBrace),
          (
            Token::OpenBrace,
            delimited::<ObjectChild<T>>(Token::OpenBrace, Token::Comma, Token::CloseBrace),
            Token::CloseBrace,
          ),
        )),
      ));
    }
  }
}

impl<T: ObjectKind + GrammarPratt> GrammarRule for ObjectChild<T> {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named(
      format!("ObjectEntry{}", T::NAME),
      choice(((
        rule::<Ident>().field("key"),
        choice((
          (Token::Colon, rule::<T>().field("value")),
          (
            choice((Token::ColonColon, (Token::Colon, Token::Colon))),
            rule::<T>().field("annotation"),
          ),
        ))
        .optional(),
      ),)),
    )
  }
}

impl<T: ObjectKind> ParsePratt<T, Object<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<Object<T>>, Diag> {
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

impl<T: ObjectKind> Parse<ObjectChild<T>> for Parser<'_>
where
  Self: Parse<T>,
{
  fn _parse(&mut self) -> Result<Option<ObjectChild<T>>, Diag> {
    let span = self.start_span();
    if let Some(key) = self.try_parse::<Spanned<Ident>>()? {
      if self.eat(Token::Colon)? {
        if let Some(value) = self.try_parse()? {
          Ok(Some(ObjectChild::Entry { key, value }))
        } else if T::ALLOW_SHORTHAND && self.eat(Token::Colon)? {
          let ty = self.parse_pratt(Prec::Annotation)?;
          let value = T::from_shorthand(self.end_span(span), key.clone(), Some(ty));
          Ok(Some(ObjectChild::Entry { key, value }))
        } else {
          self.unexpected()
        }
      } else if T::ALLOW_SHORTHAND {
        let value = T::from_shorthand(self.end_span(span), key.clone(), None);
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
