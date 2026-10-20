use super::*;

use ivy::{name::Table, text::ast::Net};
use vine_util::visit::{Visitor, VisitorMut};

new_idx!(pub Interpolation);

#[derive_(Debug, Clone)]
pub struct ExprInlineIvy {
  pub table: Table,
  pub interpolations: IdxVec<Interpolation, Expr>,
  pub net: Net<Interpolation>,
}


impl GrammarPratt for ExprInlineIvy {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, grammar::Rule)> {
    use grammar::*;
    vec![] // todo
  }
}

impl ParsePratt<Expr, ExprInlineIvy> for Parser<'_> {} // todo

impl Visit for ExprInlineIvy {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    visitor.visit(&self.interpolations);
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    visitor.visit(&mut self.interpolations);
  }
}
