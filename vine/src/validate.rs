use std::mem::take;

use crate::{
  ast::{Expr, ExprKind, Pat, PatKind},
  diag::{Diag, DiagGroup},
  visit::VisitMut,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Form {
  Value,
  Place,
  Space,
  Error,
}

#[derive(Debug, Default)]
pub struct Validate {
  pub diags: DiagGroup,
}

impl VisitMut<'_> for Validate {
  fn visit_expr(&mut self, expr: &mut Expr) {
    self.expect_expr(expr, Form::Value);
  }

  fn visit_pat(&mut self, pat: &mut Pat) {
    self.expect_pat(pat, Form::Value, false);
  }
}

impl Validate {
  fn expect_pat(&mut self, pat: &mut Pat, kind: Form, refutable: bool) {
    let span = pat.span;
    match (&mut pat.kind, kind) {
      (_, Form::Error) => unreachable!(),
      (PatKind::Error(_), _) => {}

      // (PatKind![refutable], _) if !refutable => {
      //   pat.kind = PatKind::Error(self.diags.add(Diag::ExpectedRefutablePat { span }))
      // }
      (PatKind::Hole | PatKind::Local(_) | PatKind::Adt(_, None), _) => {}
      (PatKind::Inverse(p), _) => self.expect_pat(p, invert_kind(kind), refutable),
      (PatKind::Adt(_, Some(t)) | PatKind::Tuple(t), _) => {
        for p in t {
          self.expect_pat(p, kind, refutable)
        }
      }
      (PatKind::Deref(p) | PatKind::Move(p), Form::Place) => {
        self.expect_pat(p, Form::Value, refutable)
      }
      (PatKind::Type(p, _), _) => {
        self.expect_pat(p, kind, refutable);
      }

      (PatKind::Ref(p), Form::Value | Form::Place) => self.expect_pat(p, Form::Place, refutable),

      (PatKind::Ref(pat), Form::Space) => {
        pat.kind = PatKind::Error(self.diags.add(Diag::RefSpacePat { span }))
      }
      (PatKind::Deref(pat), _) => {
        pat.kind = PatKind::Error(self.diags.add(Diag::DerefNonPlacePat { span }))
      }
      (PatKind::Move(_), _) => {
        pat.kind = PatKind::Error(self.diags.add(Diag::MoveNonPlacePat { span }))
      }
    }
  }

  fn expect_expr(&mut self, expr: &mut Expr, expected: Form) {
    let found = self.validate_expr(expr);
    self.coerce_expr(expr, found, expected);
  }

  fn coerce_expr(&mut self, expr: &mut Expr, from: Form, to: Form) {
    let span = expr.span;
    match (from, to) {
      (_, Form::Error) => unreachable!(),
      (Form::Error, _) => {}
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

  fn copy_expr(expr: &mut Expr) {
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

  fn set_expr(expr: &mut Expr) {
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

  fn move_expr(expr: &mut Expr) {
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

  fn validate_expr(&mut self, expr: &mut Expr) -> Form {
    match &mut expr.kind {
      ExprKind::Error(_) => Form::Error,
      ExprKind![synthetic] => unreachable!(),
      ExprKind![!place && !space && !error && !synthetic] => {
        self.validate_expr_value(expr);
        Form::Value
      }
      ExprKind![!value && !space && !error && !synthetic] => {
        self.validate_expr_place(expr);
        Form::Place
      }
      ExprKind::Hole => Form::Space,
      ExprKind::Inverse(e) => invert_kind(self.validate_expr(e)),
      ExprKind::Tuple(v) => {
        if v.is_empty() {
          Form::Place
        } else {
          let kinds = v.iter_mut().map(|x| self.validate_expr(x)).collect::<Vec<_>>();
          if let Some(&one_kind) = kinds.iter().find(|&&x| x != Form::Error) {
            if kinds.iter().all(|&x| x == one_kind) {
              kinds[0]
            } else {
              for (kind, e) in kinds.into_iter().zip(v) {
                self.coerce_expr(e, kind, Form::Place);
              }
              Form::Place
            }
          } else {
            Form::Error
          }
        }
      }
    }
  }

  fn validate_expr_value(&mut self, expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind![place || space || synthetic || error] => unreachable!(),

      ExprKind::U32(_)
      | ExprKind::F32(_)
      | ExprKind::String(_)
      | ExprKind::Path(_)
      | ExprKind::Continue => {}

      ExprKind::Block(..)
      | ExprKind::If(..)
      | ExprKind::While(..)
      | ExprKind::Loop(..)
      | ExprKind::For(..)
      | ExprKind::Fn(..)
      | ExprKind::Return(..)
      | ExprKind::Break(..)
      | ExprKind::List(..)
      | ExprKind::Call(..)
      | ExprKind::Neg(..)
      | ExprKind::BinaryOp(..)
      | ExprKind::Not(..)
      | ExprKind::LogicalOp(..)
      | ExprKind::ComparisonOp(..) => self._visit_expr(expr),

      ExprKind::Match(s, a) => {
        self.expect_expr(s, Form::Value);
        for (p, v) in a {
          self.expect_pat(p, Form::Value, true);
          self.expect_expr(v, Form::Value);
        }
      }
      ExprKind::Is(e, p) => {
        self.expect_expr(e, Form::Value);
        self.expect_pat(p, Form::Value, true);
      }
      ExprKind::Assign(s, v) => {
        self.expect_expr(s, Form::Space);
        self.expect_expr(v, Form::Value);
      }
      ExprKind::Ref(e) | ExprKind::Move(e) => self.expect_expr(e, Form::Place),
      ExprKind::Method(p, _, a) => {
        self.expect_expr(p, Form::Place);
        for x in a {
          self.expect_expr(x, Form::Value);
        }
      }
      ExprKind::BinaryOpAssign(_, p, v) => {
        self.expect_expr(p, Form::Place);
        self.expect_expr(v, Form::Value);
      }
    }
  }

  fn validate_expr_place(&mut self, expr: &mut Expr) {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![value || space || synthetic || error] => unreachable!(),
      ExprKind::Local(_) => {}
      ExprKind::Deref(e) => match self.validate_expr(e) {
        Form::Value | Form::Error => {}
        Form::Place => todo!(),
        Form::Space => {
          expr.kind = ExprKind::Error(self.diags.add(Diag::ExpectedValueFoundSpaceExpr { span }))
        }
      },
      ExprKind::Field(e, _) => self.expect_expr(e, Form::Place),
    }
  }
}

fn invert_kind(kind: Form) -> Form {
  match kind {
    Form::Value => Form::Space,
    Form::Place => Form::Place,
    Form::Space => Form::Value,
    Form::Error => Form::Error,
  }
}
