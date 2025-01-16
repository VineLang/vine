use crate::{
  ast::{Expr, ExprKind},
  checker::{Checker, Form},
  diag::Diag,
};

impl<'core> Checker<'core, '_> {
  pub(super) fn coerce_expr(&mut self, expr: &mut Expr<'core>, from: Form, to: Form) {
    let span = expr.span;
    match (from, to) {
      (_, Form::Error(_)) | (Form::Error(_), _) => {}
      (Form::Value, Form::Value) | (Form::Place, Form::Place) | (Form::Space, Form::Space) => {}
      // (Form::Value, Form::Place) => expr.wrap(ExprKind::Temp),
      (Form::Place, Form::Value) => Self::copy_expr(expr),
      (Form::Place, Form::Space) => Self::set_expr(expr),

      (Form::Value, Form::Place) => {
        expr.kind = ExprKind::Error(self.core.report(Diag::ExpectedPlaceFoundValueExpr { span }));
      }
      (Form::Space, Form::Value) => {
        expr.kind = ExprKind::Error(self.core.report(Diag::ExpectedValueFoundSpaceExpr { span }));
      }
      (Form::Space, Form::Place) => {
        expr.kind = ExprKind::Error(self.core.report(Diag::ExpectedPlaceFoundSpaceExpr { span }))
      }
      (Form::Value, Form::Space) => {
        expr.kind = ExprKind::Error(self.core.report(Diag::ExpectedSpaceFoundValueExpr { span }));
      }
    }
  }

  fn copy_expr(expr: &mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::CopyLocal(*l),
      ExprKind::Inverse(e, _) => Self::hedge_expr(e),
      ExprKind::Adt(_, _, _, t) | ExprKind::Tuple(t) => {
        for e in t {
          Self::copy_expr(e);
        }
      }
      ExprKind::Object(e) => {
        for (_, v) in e {
          Self::copy_expr(v);
        }
      }
      _ => expr.wrap(ExprKind::Copy),
    }
  }

  fn hedge_expr(expr: &mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::HedgeLocal(*l),
      ExprKind::Inverse(e, _) => Self::copy_expr(e),
      ExprKind::Adt(_, _, _, t) | ExprKind::Tuple(t) => {
        for e in t {
          Self::hedge_expr(e);
        }
      }
      ExprKind::Object(e) => {
        for (_, v) in e {
          Self::hedge_expr(v);
        }
      }
      _ => expr.wrap(ExprKind::Hedge),
    }
  }

  fn set_expr(expr: &mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::SetLocal(*l),
      ExprKind::Inverse(e, _) => Self::move_expr(e),
      ExprKind::Adt(_, _, _, t) | ExprKind::Tuple(t) => {
        for e in t {
          Self::set_expr(e);
        }
      }
      ExprKind::Object(e) => {
        for (_, v) in e {
          Self::set_expr(v);
        }
      }
      _ => expr.wrap(ExprKind::Set),
    }
  }

  fn move_expr(expr: &mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::Local(l) => expr.kind = ExprKind::MoveLocal(*l),
      ExprKind::Inverse(e, _) => Self::set_expr(e),
      ExprKind::Adt(_, _, _, t) | ExprKind::Tuple(t) => {
        for e in t {
          Self::move_expr(e);
        }
      }
      ExprKind::Object(e) => {
        for (_, v) in e {
          Self::move_expr(v);
        }
      }
      _ => expr.wrap(|x| ExprKind::Move(x, false)),
    }
  }
}
