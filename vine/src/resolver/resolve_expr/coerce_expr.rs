use crate::{
  diag::Diag,
  resolver::{Form, Resolver},
  tir::{TirExpr, TirExprKind},
};

impl<'core> Resolver<'core, '_> {
  pub(super) fn coerce_expr(&mut self, expr: &mut TirExpr, to: Form) {
    let span = expr.span;
    match (expr.form, to) {
      (_, Form::Error(_)) | (Form::Error(_), _) => {}
      (Form::Value, Form::Value) | (Form::Place, Form::Place) | (Form::Space, Form::Space) => {}
      (Form::Place, Form::Value) => Self::copy_expr(expr),
      (Form::Place, Form::Space) => Self::set_expr(expr),

      (Form::Value, Form::Place) => {
        *expr.kind =
          TirExprKind::Error(self.core.report(Diag::ExpectedPlaceFoundValueExpr { span }));
      }
      (Form::Space, Form::Value) => {
        *expr.kind =
          TirExprKind::Error(self.core.report(Diag::ExpectedValueFoundSpaceExpr { span }));
      }
      (Form::Space, Form::Place) => {
        *expr.kind =
          TirExprKind::Error(self.core.report(Diag::ExpectedPlaceFoundSpaceExpr { span }))
      }
      (Form::Value, Form::Space) => {
        *expr.kind =
          TirExprKind::Error(self.core.report(Diag::ExpectedSpaceFoundValueExpr { span }));
      }
    }
  }

  fn copy_expr(expr: &mut TirExpr) {
    match &mut *expr.kind {
      TirExprKind::Local(local) => *expr.kind = TirExprKind::CopyLocal(*local),
      TirExprKind::Inverse(inner) => Self::hedge_expr(inner),
      TirExprKind::Struct(_, data) => {
        Self::copy_expr(data);
      }
      TirExprKind::Composite(elements) => {
        for element in elements {
          Self::copy_expr(element);
        }
      }
      _ => *expr.kind = TirExprKind::Copy(expr.clone()),
    }
    expr.form = Form::Value;
  }

  fn hedge_expr(expr: &mut TirExpr) {
    match &mut *expr.kind {
      TirExprKind::Local(local) => *expr.kind = TirExprKind::HedgeLocal(*local),
      TirExprKind::Inverse(inner) => Self::copy_expr(inner),
      TirExprKind::Struct(_, data) => {
        Self::hedge_expr(data);
      }
      TirExprKind::Composite(elements) => {
        for element in elements {
          Self::hedge_expr(element);
        }
      }
      _ => *expr.kind = TirExprKind::Hedge(expr.clone()),
    }
    expr.form = Form::Space;
  }

  fn set_expr(expr: &mut TirExpr) {
    match &mut *expr.kind {
      TirExprKind::Local(local) => *expr.kind = TirExprKind::SetLocal(*local),
      TirExprKind::Inverse(inner) => Self::move_expr(inner),
      TirExprKind::Struct(_, data) => {
        Self::set_expr(data);
      }
      TirExprKind::Composite(elements) => {
        for element in elements {
          Self::set_expr(element);
        }
      }
      _ => *expr.kind = TirExprKind::Set(expr.clone()),
    }
    expr.form = Form::Space;
  }

  fn move_expr(expr: &mut TirExpr) {
    match &mut *expr.kind {
      TirExprKind::Local(local) => *expr.kind = TirExprKind::MoveLocal(*local),
      TirExprKind::Inverse(inner) => Self::set_expr(inner),
      TirExprKind::Struct(_, data) => {
        Self::move_expr(data);
      }
      TirExprKind::Composite(elements) => {
        for element in elements {
          Self::move_expr(element);
        }
      }
      _ => *expr.kind = TirExprKind::Move(expr.clone()),
    }
    expr.form = Form::Value;
  }
}
