use super::*;

use crate::feature::path::PathComponent;

#[derive_(Debug, Clone, Visit!)]
pub struct ItemUse {
  pub imports: Sequence<ImportTree>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct ImportTree {
  pub span: Span,
  pub component: PathComponent,
  pub kinds: Sequence<ImportKind>,
}

#[derive_(Debug, Clone, Visit!)]
pub enum ImportKind {
  Direct(Span),
  Alias(Spanned<Ident>),
  Implicit(Span),
  All(Span),
  AllImplicit(Span),
  Tree(ImportTree),
}
