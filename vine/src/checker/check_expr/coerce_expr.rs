use std::mem::take;

use crate::{
  ast::{Expr, ExprKind},
  checker::{Checker, Form},
  diag::Diag,
};

impl<'core> Checker<'core, '_> {
  pub(super) fn coerce_expr(&mut self, expr: &mut Expr<'core>, from: Form, to: Form) {
    let span = expr.span;
    match (from, to) {
      (_, Form::Error(_)) => unreachable!(),
      (Form::Error(_), _) => {}
      (Form::Value, Form::Value) | (Form::Place, Form::Place) | (Form::Space, Form::Space) => {}
      (Form::Value, Form::Place) => expr.wrap(ExprKind::Temp),
      (Form::Place, Form::Value) => Self::copy_expr(expr),
      (Form::Place, Form::Space) => Self::set_expr(expr),

      (Form::Space, Form::Value) => {
        expr.kind = ExprKind::Error(self.diags.add(Diag::ExpectedValueFoundSpaceExpr { span }));
      }
      (Form::Space, Form::Place) => {
        expr.kind = ExprKind::Error(self.diags.add(Diag::ExpectedPlaceFoundSpaceExpr { span }))
      }
      (Form::Value, Form::Space) => {
        expr.kind = ExprKind::Error(self.diags.add(Diag::ExpectedSpaceFoundValueExpr { span }));
      }
    }
  }

  fn copy_expr(expr: &mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::CopyLocal(*l),
      ExprKind::Tuple(t) => {
        for e in t {
          Self::copy_expr(e);
        }
      }
      ExprKind::Temp(t) => {
        expr.kind = take(&mut t.kind);
      }
      _ => expr.wrap(ExprKind::Copy),
    }
  }

  fn set_expr(expr: &mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::SetLocal(*l),
      ExprKind::Inverse(e) => Self::move_expr(e),
      ExprKind::Tuple(t) => {
        for e in t {
          Self::set_expr(e);
        }
      }
      _ => expr.wrap(ExprKind::Set),
    }
  }

  fn move_expr(expr: &mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::MoveLocal(*l),
      ExprKind::Inverse(e) => Self::set_expr(e),
      ExprKind::Tuple(t) => {
        for e in t {
          Self::move_expr(e);
        }
      }
      _ => expr.wrap(ExprKind::Move),
    }
  }
}
