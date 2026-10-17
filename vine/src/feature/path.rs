use super::*;

use crate::feature::generics::GenericArgs;

#[derive_(Debug, Clone, Visit!)]
pub struct PathComponent {
  pub span: Span,
  pub stock: bool,
  pub ident: Ident,
}

#[derive_(Debug, Clone, Visit!)]
pub struct Path {
  pub components: Vec<PathComponent>,
  pub generics: GenericArgs,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ExprPath {
  pub path: Path,
  pub args: Option<Sequence<Expr>>,
}

impl ParsePratt<Expr, ExprPath> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct PatPath {
  pub path: Path,
  pub args: Option<Sequence<Expr>>,
}

impl ParsePratt<Pat, PatPath> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TyPath {
  pub path: Path,
}

impl ParsePratt<Ty, TyPath> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct TraitPath {
  pub path: Path,
}

impl ParsePratt<Trait, TraitPath> for Parser<'_> {} // todo

#[derive_(Debug, Clone, Visit!)]
pub struct ImplPath {
  pub path: Path,
}

impl ParsePratt<Impl, ImplPath> for Parser<'_> {} // todo
