use super::*;

use crate::feature::generics::GenericArgs;

#[derive_(Debug, Clone, Visit!)]
pub struct PathComponent {
  pub span: LocalSpan,
  pub stock: bool,
  pub ident: Ident,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Path {
  pub span: LocalSpan,
  pub components: Vec<PathComponent>,
  pub generics: Option<GenericArgs>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprPath {
  pub path: Path,
  pub args: Option<Sequence<Expr>>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatPath {
  pub path: Path,
  pub args: Option<Sequence<Expr>>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TyPath {
  pub path: Path,
}

#[derive_(Debug, Clone, Visit!)]
pub struct TraitPath {
  pub path: Path,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ImplPath {
  pub path: Path,
}

impl Parse<PathComponent> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<PathComponent>, Diag> {
    let span = self.start_span();
    let stock = self.eat(Token::Hash)?;
    if let Some(ident) = self.try_parse()? {
      let span = self.end_span(span);
      Ok(Some(PathComponent { span, stock, ident }))
    } else if stock {
      self.unexpected()
    } else {
      Ok(None)
    }
  }
}

impl Parse<Path> for Parser<'_> {
  fn _parse(&mut self) -> Result<Option<Path>, Diag> {
    let span = self.start_span();
    if let Some(component) = self.try_parse()? {
      let mut components = vec![component];
      while self.eat(Token::ColonColon)? {
        components.push(self.parse()?);
      }
      let generics = self.check_then(Token::OpenBracket, Self::parse)?;
      let span = self.end_span(span);
      Ok(Some(Path { span, components, generics }))
    } else {
      Ok(None)
    }
  }
}

impl ParsePratt<Expr, ExprPath> for Parser<'_> {} // todo
impl ParsePratt<Pat, PatPath> for Parser<'_> {} // todo
impl ParsePratt<Ty, TyPath> for Parser<'_> {} // todo
impl ParsePratt<Trait, TraitPath> for Parser<'_> {} // todo
impl ParsePratt<Impl, ImplPath> for Parser<'_> {} // todo

impl From<Spanned<Ident>> for PathComponent {
  fn from(value: Spanned<Ident>) -> Self {
    PathComponent { span: value.span, stock: false, ident: value.inner }
  }
}

impl From<Spanned<Ident>> for Path {
  fn from(value: Spanned<Ident>) -> Self {
    Path { span: value.span, components: vec![value.into()], generics: Default::default() }
  }
}
