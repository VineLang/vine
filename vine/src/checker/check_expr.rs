use std::mem::take;

use crate::{
  ast::{BinaryOp, Expr, ExprKind, GenericPath, Span},
  checker::{Checker, Form, Type},
  diag::Diag,
};

use super::report;

mod coerce_expr;

impl Checker<'_> {
  pub(super) fn check_expr_form_type(&mut self, expr: &mut Expr, form: Form, ty: &mut Type) {
    let mut found = self.check_expr_form(expr, form);
    if !self.unify(&mut found, ty) {
      self.diags.add(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.display_type(ty),
        found: self.display_type(&found),
      });
    }
  }

  pub(super) fn check_expr_form(&mut self, expr: &mut Expr, form: Form) -> Type {
    let (found, ty) = self.check_expr(expr);
    self.coerce_expr(expr, found, form);
    ty
  }

  pub(super) fn check_expr(&mut self, expr: &mut Expr) -> (Form, Type) {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![synthetic] => unreachable!(),
      ExprKind![!error && !space && !place && !synthetic] => {
        (Form::Value, self._check_expr_value(expr))
      }
      ExprKind::Error(e) => (Form::Error(*e), Type::Error(*e)),
      ExprKind::Hole => (Form::Space, self.new_var(span)),
      ExprKind::Local(l) => (
        Form::Place,
        Type::Var(*self.state.locals.entry(*l).or_insert_with(|| {
          let v = self.state.vars.len();
          self.state.vars.push(Err(span));
          v
        })),
      ),
      ExprKind::Deref(_) => todo!(),
      ExprKind::Inverse(expr) => {
        let (form, ty) = self.check_expr(expr);
        (form.inverse(), ty.inverse())
      }
      ExprKind::Tuple(v) => {
        if v.is_empty() {
          (Form::Place, Type::UNIT)
        } else {
          let (forms, types) =
            v.iter_mut().map(|x| self.check_expr(x)).collect::<(Vec<_>, Vec<_>)>();
          let form = if let Some(&one_form) = forms.iter().find(|x| !matches!(x, Form::Error(_))) {
            if forms.iter().all(|&x| matches!(x, Form::Error(_)) || x == one_form) {
              one_form
            } else {
              for (form, e) in forms.into_iter().zip(v) {
                self.coerce_expr(e, form, Form::Place);
              }
              Form::Place
            }
          } else {
            forms[0]
          };
          (form, Type::Tuple(types))
        }
      }
      ExprKind::Field(..) => todo!(),
    }
  }

  fn _check_expr_value(&mut self, expr: &mut Expr) -> Type {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![error || place || space || synthetic] => unreachable!(),
      ExprKind::Path(path) => report!(self.diags, expr.kind; self.typeof_value_def(path)),
      ExprKind::Block(block) => self.check_block(block),
      ExprKind::Assign(space, value) => {
        let mut ty = self.check_expr_form(space, Form::Space);
        self.check_expr_form_type(value, Form::Value, &mut ty);
        Type::UNIT
      }
      ExprKind::Match(scrutinee, arms) => {
        let mut scrutinee = self.check_expr_form(scrutinee, Form::Value);
        let mut result = self.new_var(span);
        for (pat, expr) in arms {
          self.check_pat_type(pat, Form::Value, true, &mut scrutinee);
          self.check_expr_form_type(expr, Form::Value, &mut result);
        }
        result
      }
      ExprKind::If(cond, then, els) => {
        self.check_expr_form_type(cond, Form::Value, &mut Type::U32);
        let mut then = self.check_block(then);
        let mut els = self.check_expr_form(els, Form::Value);
        if !self.unify(&mut then, &mut els) {
          Type::Error(self.diags.add(Diag::MismatchedThenElseTypes {
            span,
            then: self.display_type(&then),
            els: self.display_type(&els),
          }))
        } else {
          then
        }
      }
      ExprKind::While(cond, block) => {
        let old = self.loop_ty.replace(Type::UNIT);
        self.check_expr_form_type(cond, Form::Value, &mut Type::U32);
        self.check_block(block);
        self.loop_ty = old;
        Type::UNIT
      }
      ExprKind::Loop(block) => {
        let result = self.new_var(span);
        let old = self.loop_ty.replace(result.clone());
        self.check_block(block);
        self.loop_ty = old;
        result
      }
      ExprKind::Fn(args, ret, body) => Type::Fn(
        args
          .iter_mut()
          .map(|(pat, ty)| self.check_pat_annotation(pat, ty.as_mut(), Form::Value, false))
          .collect(),
        Box::new({
          let mut ret = match ret {
            Some(Some(t)) => self.hydrate_type(t, true),
            Some(None) => Type::UNIT,
            None => self.new_var(span),
          };
          let old = self.return_ty.replace(ret.clone());
          self.check_expr_form_type(body, Form::Value, &mut ret);
          self.return_ty = old;
          ret
        }),
      ),
      ExprKind::Break(e) => {
        if let Some(ty) = &self.loop_ty {
          let mut ty = ty.clone();
          if let Some(e) = e {
            self.check_expr_form_type(e, Form::Value, &mut ty);
          } else if !self.unify(&mut ty, &mut { Type::UNIT }) {
            self.diags.add(Diag::MissingBreakExpr { span, ty: self.display_type(&ty) });
          }
        } else {
          self.diags.add(Diag::NoLoopBreak { span });
        }
        self.new_var(span)
      }
      ExprKind::Return(e) => {
        if let Some(ty) = &self.return_ty {
          let mut ty = ty.clone();
          if let Some(e) = e {
            self.check_expr_form_type(e, Form::Value, &mut ty);
          } else if !self.unify(&mut ty, &mut { Type::UNIT }) {
            self.diags.add(Diag::MissingReturnExpr { span, ty: self.display_type(&ty) });
          }
        } else {
          self.diags.add(Diag::NoReturn { span });
        }
        self.new_var(span)
      }
      ExprKind::Continue => {
        if self.loop_ty.is_none() {
          self.diags.add(Diag::NoLoopContinue { span });
        }
        self.new_var(span)
      }
      ExprKind::Ref(expr) => {
        let ty = self.check_expr_form(expr, Form::Place);
        Type::Ref(Box::new(ty))
      }
      ExprKind::Move(expr) => self.check_expr_form(expr, Form::Place),
      ExprKind::List(els) => {
        let mut item = self.new_var(span);
        for el in els {
          self.check_expr_form_type(el, Form::Value, &mut item);
        }
        if let Some(list) = self.list {
          Type::Adt(list, vec![item])
        } else {
          Type::Error(self.diags.add(Diag::NoList { span }))
        }
      }
      ExprKind::Method(receiver, path, args) => self.check_method(span, receiver, path, args),
      ExprKind::Call(func, args) => self.check_call(func, args, span),
      ExprKind::Not(expr) => {
        self.check_expr_form_type(expr, Form::Value, &mut Type::U32);
        Type::U32
      }
      ExprKind::Is(expr, pat) => {
        let mut ty = self.check_expr_form(expr, Form::Value);
        self.check_pat_type(pat, Form::Value, true, &mut ty);
        Type::U32
      }
      ExprKind::LogicalOp(_, a, b) => {
        self.check_expr_form_type(a, Form::Value, &mut Type::U32);
        self.check_expr_form_type(b, Form::Value, &mut Type::U32);
        Type::U32
      }
      ExprKind::Neg(expr) => {
        let ty = self.check_expr_form(expr, Form::Value);
        self.check_bin_op(span, BinaryOp::Sub, Type::U32, ty)
      }
      ExprKind::BinaryOp(op, a, b) => {
        let a = self.check_expr_form(a, Form::Value);
        let b = self.check_expr_form(b, Form::Value);
        self.check_bin_op(span, *op, a, b)
      }
      ExprKind::BinaryOpAssign(op, a, b) => {
        let mut a = self.check_expr_form(a, Form::Place);
        let b = self.check_expr_form(b, Form::Value);
        let mut o = self.check_bin_op(span, *op, a.clone(), b);
        self.unify(&mut a, &mut o);
        Type::UNIT
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut lhs = self.check_expr_form(init, Form::Value);
        self.concretize(&mut lhs);
        for (_, next) in cmps {
          let mut rhs = self.check_expr_form(next, Form::Value);
          self.concretize(&mut rhs);
          if !matches!(
            (&lhs, &rhs),
            (Type::U32, Type::U32)
              | (Type::F32, Type::F32)
              | (Type::Error(_), _)
              | (_, Type::Error(_))
          ) {
            self.diags.add(Diag::CannotCompare {
              span,
              lhs: self.display_type(&lhs),
              rhs: self.display_type(&rhs),
            });
          }
          lhs = rhs;
        }
        Type::U32
      }
      ExprKind::U32(_) => Type::U32,
      ExprKind::F32(_) => Type::F32,
      ExprKind::String(_) => report!(self.diags; self.string.clone().ok_or(Diag::NoList { span })),
      ExprKind::For(..) => todo!(),
    }
  }

  fn check_method(
    &mut self,
    span: Span,
    receiver: &mut Box<Expr>,
    path: &mut GenericPath,
    args: &mut Vec<Expr>,
  ) -> Type {
    match self.method_sig(span, path, args.len()) {
      Ok((mut rec, params, ret)) => {
        self.check_expr_form_type(receiver, Form::Place, &mut rec);
        for (mut ty, arg) in params.into_iter().skip(1).zip(args) {
          self.check_expr_form_type(arg, Form::Value, &mut ty);
        }
        ret
      }
      Err(e) => {
        self.check_expr_form(receiver, Form::Place);
        for arg in args {
          self.check_expr_form(arg, Form::Value);
        }
        Type::Error(self.diags.add(e))
      }
    }
  }

  fn method_sig(
    &mut self,
    span: Span,
    path: &mut GenericPath,
    args: usize,
  ) -> Result<(Type, Vec<Type>, Type), Diag> {
    let ty = self.typeof_value_def(path)?;
    match ty {
      Type::Fn(mut params, ret) => match params.first_mut() {
        Some(Type::Ref(receiver)) => {
          let receiver = take(&mut **receiver);
          if params.len() != args + 1 {
            Err(Diag::BadArgCount {
              span,
              expected: params.len(),
              got: args,
              ty: self.display_type(&Type::Fn(params, ret)),
            })
          } else {
            Ok((receiver, params, *ret))
          }
        }
        Some(Type::Error(e)) => Err((*e).into()),
        _ => Err(Diag::NonMethodFunction { span, ty: self.display_type(&Type::Fn(params, ret)) }),
      },
      Type::Error(e) => Err(e.into()),
      ty => Err(Diag::NonFunctionCall { span, ty: self.display_type(&ty) }),
    }
  }

  fn check_call(&mut self, func: &mut Box<Expr>, args: &mut Vec<Expr>, span: Span) -> Type {
    let ty = self.check_expr_form(func, Form::Value);
    match self.fn_sig(span, ty, args.len()) {
      Ok((params, ret)) => {
        for (mut ty, arg) in params.into_iter().zip(args) {
          self.check_expr_form_type(arg, Form::Value, &mut ty);
        }
        ret
      }
      Err(e) => {
        for arg in args {
          self.check_expr_form(arg, Form::Value);
        }
        Type::Error(self.diags.add(e))
      }
    }
  }

  fn fn_sig(&mut self, span: Span, mut ty: Type, args: usize) -> Result<(Vec<Type>, Type), Diag> {
    self.concretize(&mut ty);
    match ty {
      Type::Fn(params, ret) => {
        if params.len() != args {
          Err(Diag::BadArgCount {
            span,
            expected: params.len(),
            got: args,
            ty: self.display_type(&Type::Fn(params, ret)),
          })
        } else {
          Ok((params, *ret))
        }
      }
      Type::Error(e) => Err(e.into()),
      ty => Err(Diag::NonFunctionCall { span, ty: self.display_type(&ty) }),
    }
  }

  fn check_bin_op(&mut self, span: Span, op: BinaryOp, mut a: Type, mut b: Type) -> Type {
    match op {
      BinaryOp::Range | BinaryOp::RangeTo => todo!(),
      BinaryOp::Concat => {
        self.concretize(&mut a);
        self.concretize(&mut b);
        if self.unify(&mut a, &mut b) {
          if let Type::Adt(n, _) = a {
            if Some(n) == self.list {
              return a;
            }
          }
        }
      }
      _ => {
        if let Ok(ty) = self._check_bin_op(op, &mut a, &mut b, false) {
          return ty;
        }
      }
    }
    let diag = Diag::BadBinOp { span, op, lhs: self.display_type(&a), rhs: self.display_type(&b) };
    Type::Error(self.diags.add(diag))
  }

  fn _check_bin_op(
    &mut self,
    op: BinaryOp,
    mut a: &mut Type,
    mut b: &mut Type,
    i: bool,
  ) -> Result<Type, ()> {
    self.concretize(a);
    self.concretize(b);
    match (op, &mut a, &mut b) {
      (_, Type::Error(e), _) | (_, _, Type::Error(e)) => Ok(Type::Error(*e)),
      (_, Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => a
        .iter_mut()
        .zip(b)
        .map(|(a, b)| self._check_bin_op(op, a, b, i))
        .collect::<Result<_, _>>()
        .map(Type::Tuple),
      (_, Type::Tuple(a), b @ (Type::U32 | Type::F32)) => a
        .iter_mut()
        .map(|a| self._check_bin_op(op, a, b, i))
        .collect::<Result<_, _>>()
        .map(Type::Tuple),
      (_, a @ (Type::U32 | Type::F32), Type::Tuple(b)) => b
        .iter_mut()
        .map(|b| self._check_bin_op(op, a, b, i))
        .collect::<Result<_, _>>()
        .map(Type::Tuple),
      (_, Type::U32, Type::U32) if !i => Ok(Type::U32),
      (
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem,
        Type::U32 | Type::F32,
        Type::U32 | Type::F32,
      ) if !i => Ok(Type::F32),
      (_, Type::Inverse(a), b) => self._check_bin_op(op, a, b, !i),
      (_, a, Type::Inverse(b)) => self._check_bin_op(op, a, b, !i),
      _ => Err(()),
    }
  }
}