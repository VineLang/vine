use super::*;

use crate::feature::{generics::GenericArgs, sequence::ParseSequence};

#[derive_(Debug, Clone, Visit!)]
pub struct PathComponent {
  pub span: LocalSpan,
  pub stock: bool,
  pub ident: Ident,
  pub resolved: Resolve<!>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Path<R> {
  pub span: LocalSpan,
  pub components: Vec<PathComponent>,
  pub generics: Option<GenericArgs>,
  pub resolved: Resolve<R>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PathCall<R, T> {
  pub path: Path<R>,
  pub args: Option<Sequence<T>>,
}

pub type ExprPathCall = PathCall<!, Expr>;
pub type PatPathCall = PathCall<!, Pat>;
pub type TyPath = Path<!>;
pub type TraitPath = Path<!>;
pub type ImplPath = Path<!>;

impl GrammarRule for PathComponent {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    grammar.named("PathComponent", (Token::Hash.optional(), rule::<Ident>()))
  }
}

impl GrammarRule for Path<!> {
  fn _rule(grammar: &mut Grammar) -> grammar::Rule {
    use grammar::*;
    // todo
    grammar.named(
      "Path",
      (
        (rule::<PathComponent>(), (Token::ColonColon, rule::<PathComponent>()).repeat()),
        rule::<GenericArgs>().field("generics"),
      ),
    )
  }
}

impl<R, T: ParseSequence> GrammarPrattPart for PathCall<R, T> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl<T> GrammarPrattPart for Path<T> {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![(Prec::Max, grammar.named("Path", rule::<Path<!>>()))] // todo
  }
}

impl Parse<PathComponent> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<PathComponent>, Diag> {
    let span = self.start_span();
    let stock = self.eat(Token::Hash)?;
    if let Some(ident) = self.try_parse()? {
      let span = self.end_span(span);
      Ok(Some(PathComponent { span, stock, ident, resolved: Unresolved }))
    } else if stock {
      self.unexpected()
    } else {
      Ok(None)
    }
  }
}

impl<R> Parse<Path<R>> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Path<R>>, Diag> {
    let span = self.start_span();
    if let Some(component) = self.try_parse()? {
      let mut components = vec![component];
      while self.eat(Token::ColonColon)? {
        components.push(self.parse()?);
      }
      let generics = self.check_then(Token::OpenBracket, Self::parse)?;
      let span = self.end_span(span);
      Ok(Some(Path { span, components, generics, resolved: Unresolved }))
    } else {
      Ok(None)
    }
  }
}

impl<R, T: ParseSequence> ParsePratt<T, PathCall<R, T>> for Parser<'_>
where
  Self: ParsePratt<T>,
{
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<PathCall<R, T>>, Diag> {
    if let Some(path) = self.try_parse::<Path<R>>()? {
      let args = self.try_parse::<Sequence<T>>()?;
      Ok(Some(PathCall { path, args }))
    } else {
      Ok(None)
    }
  }
}

impl ParsePratt<Ty, TyPath> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<TyPath>, Diag> {
    self.try_parse()
  }
}

impl ParsePratt<Trait, TraitPath> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<TraitPath>, Diag> {
    self.try_parse()
  }
}

impl ParsePratt<Impl, ImplPath> for Parser<'_> {
  fn _parse_initial(&mut self, _: &mut PrecCtx) -> Result<Option<ImplPath>, Diag> {
    self.try_parse()
  }
}

impl From<Spanned<Ident>> for PathComponent {
  fn from(value: Spanned<Ident>) -> Self {
    PathComponent { span: value.span, stock: false, ident: value.inner, resolved: Unresolved }
  }
}

impl<R> From<Spanned<Ident>> for Path<R> {
  fn from(value: Spanned<Ident>) -> Self {
    Path { span: value.span, components: vec![value.into()], generics: None, resolved: Unresolved }
  }
}

impl<R> Path<R> {
  pub fn as_ident(&self) -> Option<Spanned<Ident>> {
    if self.generics.is_none()
      && let [component] = &*self.components
      && !component.stock
    {
      Some(Spanned { span: component.span, inner: component.ident.clone() })
    } else {
      None
    }
  }
}
