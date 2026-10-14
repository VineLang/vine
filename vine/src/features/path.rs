use crate::structures::ast::{Expr, IdentValue, Sequence, Span};

#[derive(Debug, Clone)]
pub struct PathComponent {
  pub span: Span,
  pub stock: bool,
  pub ident: IdentValue,
}

#[derive(Debug, Clone)]
pub struct Path {
  pub components: Vec<PathComponent>,
  pub generics: GenericArgs,
}

#[derive(Debug, Clone)]
pub struct ExprPath {
  pub path: Path,
  pub args: Option<Sequence<Expr>>,
}

#[derive(Debug, Clone)]
pub struct PatPath {
  pub path: Path,
  pub args: Option<Sequence<Expr>>,
}

#[derive(Debug, Clone)]
pub struct TyPath {
  pub path: Path,
}

#[derive(Debug, Clone)]
pub struct TraitPath {
  pub path: Path,
}

#[derive(Debug, Clone)]
pub struct ImplPath {
  pub path: Path,
}
