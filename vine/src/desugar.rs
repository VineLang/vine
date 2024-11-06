use std::mem::take;

use crate::{
  ast::{Expr, ExprKind},
  resolve::Def,
  visit::VisitMut,
};

pub struct Desugar;

impl VisitMut<'_> for Desugar {
  fn visit_def(&mut self, def: &mut Def) {
    self._visit_def(def)
  }

  fn visit_expr(&mut self, expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind::Field(receiver, path) => {
        let o = take(&mut **receiver);
        let path = take(path);
        expr.kind = ExprKind::Deref(Box::new(Expr {
          span: expr.span,
          kind: ExprKind::Call(
            Box::new(Expr { span: path.span, kind: ExprKind::Path(path) }),
            vec![Expr { span: o.span, kind: ExprKind::Ref(Box::new(o)) }],
          ),
        }))
      }
      ExprKind::Method(..) => unreachable!(),
      ExprKind![!sugar] => {}
    }
    self._visit_expr(expr)
  }
}
