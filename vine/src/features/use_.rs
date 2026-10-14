use crate::{
  features::path::PathComponent,
  structures::ast::{Span, SpanIdent},
};

#[derive(Debug, Clone)]
pub struct ItemUse {
  pub imports: Vec<ImportTree>,
}

#[derive(Debug, Clone)]
pub struct ImportTree {
  pub span: Span,
  pub component: PathComponent,
  pub kinds: Vec<ImportKind>,
}

#[derive(Debug, Clone)]
pub enum ImportKind {
  Direct(Span),
  Alias(SpanIdent),
  Implicit(Span),
  All(Span),
  AllImplicit(Span),
  Tree(ImportTree),
}
