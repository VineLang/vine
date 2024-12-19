use crate::{
  ast::{Expr, ExprKind},
  resolver::Def,
  visit::VisitMut,
};

pub struct Desugar;

impl VisitMut<'_, '_> for Desugar {
  fn visit_def(&mut self, def: &mut Def) {
    self._visit_def(def)
  }

  fn visit_expr(&mut self, expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind::Method(..) => unreachable!(),
      ExprKind![!sugar] => {}
    }
    self._visit_expr(expr)
  }
}
