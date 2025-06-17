use crate::{
  components::resolver::{Form, Resolver},
  structures::{
    diag::Diag,
    tir::{TirExpr, TirExprKind},
  },
};

impl<'core> Resolver<'core, '_> {
  pub(super) fn coerce_expr(&mut self, expr: &mut TirExpr, to: Form) {
    if let Err(diag) = self._coerce_expr(expr, to) {
      let err = self.core.report(diag);
      expr.form = Form::Error(err);
      *expr.kind = TirExprKind::Error(err);
    }
  }

  fn _coerce_expr(&mut self, expr: &mut TirExpr, to: Form) -> Result<(), Diag<'core>> {
    let span = expr.span;
    match (expr.form, to) {
      (_, Form::Error(err)) | (Form::Error(err), _) => Err(err)?,
      (Form::Value, Form::Value) | (Form::Place, Form::Place) | (Form::Space, Form::Space) => {}
      (Form::Place, Form::Value) => Self::copy_expr(expr),
      (Form::Place, Form::Space) => Self::set_expr(expr),

      (Form::Value, Form::Place) => Err(Diag::ExpectedPlaceFoundValueExpr { span })?,
      (Form::Space, Form::Value) => Err(Diag::ExpectedValueFoundSpaceExpr { span })?,
      (Form::Space, Form::Place) => Err(Diag::ExpectedPlaceFoundSpaceExpr { span })?,
      (Form::Value, Form::Space) => Err(Diag::ExpectedSpaceFoundValueExpr { span })?,
    }
    Ok(())
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
