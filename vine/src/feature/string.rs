use super::*;

use vine_util::visit::{Visitor, VisitorMut};

#[derive_(Debug, Clone, Visit!)]
pub struct ExprString {
  pub string: StringLiteral<Expr>,
}

#[derive_(Debug, Clone, Visit!)]
pub struct PatString {
  pub string: StringLiteral<Pat>,
}

#[derive_(Debug, Clone)]
pub struct StringLiteral<T = !> {
  pub span: Span,
  pub kind: StringKind,
  pub lang: Option<String>,
  pub lines: Vec<StringLine<T>>,
}

#[derive_(Debug, Clone)]
pub enum StringKind {
  Raw,
  Processed,
}

#[derive_(Debug, Clone)]
pub struct StringLine<T = !> {
  pub pieces: Vec<StringPiece<T>>,
}

#[derive_(Debug, Clone)]
pub enum StringPiece<T = !> {
  Segment(String),
  Escape(char),
  Interpolation(T),
}

impl<T: Visit> Visit for StringLiteral<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    visitor.visit(&self.span);
    visitor.visit(&self.lines);
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    visitor.visit(&mut self.span);
    visitor.visit(&mut self.lines);
  }
}

impl<T: Visit> Visit for StringLine<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    visitor.visit(&self.pieces);
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    visitor.visit(&mut self.pieces);
  }
}

impl<T: Visit> Visit for StringPiece<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    match self {
      StringPiece::Interpolation(child) => visitor.visit(child),
      StringPiece::Segment(_) | StringPiece::Escape(_) => {}
    }
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    match self {
      StringPiece::Interpolation(child) => visitor.visit(child),
      StringPiece::Segment(_) | StringPiece::Escape(_) => {}
    }
  }
}
