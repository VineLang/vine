use std::mem::take;

use crate::{
  ast::{Block, Expr, ExprKind, Pat, PatKind, Span, Stmt, StmtKind},
  resolve::Node,
  visit::VisitMut,
};

pub struct Desugar;

impl VisitMut<'_> for Desugar {
  fn visit_node(&mut self, node: &mut Node) {
    self._visit_node(node)
  }

  fn visit_expr(&mut self, expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind::Method(receiver, path, args) => {
        let receiver = take(&mut **receiver);
        let path = take(path);
        let mut args = take(args);
        args.insert(0, Expr { span: receiver.span, kind: ExprKind::Ref(Box::new(receiver)) });
        expr.kind =
          ExprKind::Call(Box::new(Expr { span: path.span, kind: ExprKind::Path(path) }), args);
      }
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
      ExprKind::WhileLet(pat, value, body) => {
        let pat = take(&mut **pat);
        let value = take(&mut **value);
        let body = take(body);
        expr.kind = ExprKind::Loop(Block {
          span: expr.span,
          stmts: vec![Stmt {
            span: expr.span,
            kind: StmtKind::Expr(
              Expr {
                span: expr.span,
                kind: ExprKind::Match(
                  Box::new(value),
                  vec![
                    (pat, Expr { span: body.span, kind: ExprKind::Block(body) }),
                    (
                      Pat { span: Span::NONE, kind: PatKind::Hole },
                      Expr { span: Span::NONE, kind: ExprKind::Break },
                    ),
                  ],
                ),
              },
              true,
            ),
          }],
        });
      }
      ExprKind!(!sugar) => {}
    }
    self._visit_expr(expr)
  }
}
