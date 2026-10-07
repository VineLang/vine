use vine_util::{apply, for_enum};

#[derive(Debug, Clone, Copy)]
struct Type();
#[derive(Debug, Clone, Copy)]
struct Span();

#[derive(Debug, Clone, Copy)]
pub enum Resolve<T> {
  Resolved(T),
  Unresolved,
}

pub use Resolve::*;

#[derive(Debug, Clone)]
pub struct Expr {
  span: Span,
  kind: Box<ExprKind>,
  ty: Resolve<Type>,
}

#[apply(for_enum!)]
#[derive(Debug, Clone)]
pub enum ExprKind {}
