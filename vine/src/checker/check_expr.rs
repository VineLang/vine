use std::{collections::BTreeMap, mem::take};

use crate::{
  ast::{BinaryOp, Expr, ExprKind, Generics, Label, Span},
  chart::VariantId,
  checker::{Checker, Form, Type},
  diag::Diag,
};

mod check_method;
mod coerce_expr;

impl<'core> Checker<'core, '_> {
  pub(super) fn check_expr_form_type(
    &mut self,
    expr: &mut Expr<'core>,
    form: Form,
    ty: &mut Type<'core>,
  ) {
    let mut found = self.check_expr_form(expr, form);
    if !self.unifier.unify(&mut found, ty) {
      self.core.report(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.display_type(ty),
        found: self.display_type(&found),
      });
    }
  }

  pub(super) fn check_expr_form(&mut self, expr: &mut Expr<'core>, form: Form) -> Type<'core> {
    let (found, ty) = self.check_expr(expr);
    self.coerce_expr(expr, found, form);
    ty
  }

  pub(super) fn check_expr(&mut self, expr: &mut Expr<'core>) -> (Form, Type<'core>) {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![synthetic] => unreachable!(),
      ExprKind![!error && !space && !place && !synthetic] => {
        (Form::Value, self._check_expr_value(expr))
      }
      ExprKind::Paren(e) => self.check_expr(e),
      ExprKind::Error(e) => (Form::Error(*e), Type::Error(*e)),
      ExprKind::Hole => (Form::Space, self.unifier.new_var(span)),
      ExprKind::Local(l) => (
        Form::Place,
        Type::Var(*self.locals.entry(*l).or_insert_with(|| self.unifier._new_var(span))),
      ),
      ExprKind::Deref(e, _) => {
        let v = self.unifier.new_var(span);
        self.check_expr_form_type(e, Form::Value, &mut Type::Ref(Box::new(v.clone())));
        (Form::Place, v)
      }
      ExprKind::Inverse(expr, _) => {
        let (form, ty) = self.check_expr(expr);
        (form.inverse(), ty.inverse())
      }
      ExprKind::Place(v, s) => {
        let mut v = self.check_expr_form(v, Form::Value);
        let mut s = self.check_expr_form(s, Form::Space);
        let ty = if !self.unifier.unify(&mut v, &mut s) {
          Type::Error(self.core.report(Diag::MismatchedValueSpaceTypes {
            span,
            value: self.display_type(&v),
            space: self.display_type(&s),
          }))
        } else {
          v
        };
        (Form::Place, ty)
      }
      ExprKind::TupleField(e, i, l) => {
        let (form, mut ty) = self.check_expr(e);

        if form == Form::Space {
          let e = self.core.report(Diag::SpaceField { span });
          return (Form::Error(e), Type::Error(e));
        }

        self.unifier.concretize(&mut ty);
        let field_ty = match &ty {
          Type::Tuple(t) => {
            *l = Some(t.len());
            t.get(*i).cloned()
          }
          Type::Adt(adt_id, type_params) => {
            let adt = &self.chart.adts[*adt_id];
            if adt.is_struct {
              let variant = &adt.variants[VariantId(0)];
              if !variant.object && *i < variant.fields.len() {
                *l = Some(variant.fields.len());
                Some(self.types.adt_types[*adt_id][VariantId(0)][*i].instantiate(type_params))
              } else {
                None
              }
            } else {
              None
            }
          }
          _ => None,
        };

        let field_ty = field_ty.unwrap_or_else(|| {
          Type::Error(self.core.report(Diag::MissingTupleField {
            span,
            ty: self.display_type(&ty),
            i: *i,
          }))
        });

        (form, field_ty)
      }
      ExprKind::ObjectField(e, k) => {
        let (form, mut ty) = self.check_expr(e);

        if form == Form::Space {
          let e = self.core.report(Diag::SpaceField { span });
          return (Form::Error(e), Type::Error(e));
        }

        self.unifier.concretize(&mut ty);
        let field = match &ty {
          Type::Object(e) => e
            .iter()
            .enumerate()
            .find(|&(_, (&n, _))| k.ident == n)
            .map(|(i, (_, t))| (t.clone(), i, e.len())),
          Type::Adt(adt_id, type_params) => {
            let adt = &self.chart.adts[*adt_id];
            if adt.is_struct && adt.variants[VariantId(0)].object {
              let [Type::Object(fields)] = &*self.types.adt_types[*adt_id][VariantId(0)] else {
                unreachable!()
              };
              fields
                .iter()
                .enumerate()
                .find(|&(_, (&n, _))| k.ident == n)
                .map(|(i, (_, ty))| (ty.instantiate(type_params), i, fields.len()))
            } else {
              None
            }
          }
          _ => None,
        };

        let (field_ty, i, len) = field.unwrap_or_else(|| {
          (
            Type::Error(self.core.report(Diag::MissingObjectField {
              span: k.span,
              ty: self.display_type(&ty),
              key: k.ident,
            })),
            0,
            0,
          )
        });

        expr.kind = ExprKind::TupleField(take(e), i, Some(len));

        (form, field_ty)
      }
      ExprKind::Tuple(v) => {
        if v.is_empty() {
          (Form::Place, Type::NIL)
        } else {
          let (forms, types) =
            v.iter_mut().map(|x| self.check_expr(x)).collect::<(Vec<_>, Vec<_>)>();
          let form = self.tuple_form(span, &forms);
          for (f, e) in forms.into_iter().zip(v) {
            self.coerce_expr(e, f, form);
          }
          (form, Type::Tuple(types))
        }
      }
      ExprKind::Object(entries) => {
        if entries.is_empty() {
          (Form::Place, Type::Object(BTreeMap::new()))
        } else {
          let mut forms = Vec::new();
          let ty = self.build_object_type(entries, |self_, e| {
            let (form, ty) = self_.check_expr(e);
            forms.push(form);
            ty
          });
          let form = self.tuple_form(span, &forms);
          for (f, (_, v)) in forms.into_iter().zip(entries) {
            self.coerce_expr(v, f, form);
          }
          (
            form,
            match ty {
              Ok(ty) => ty,
              Err(diag) => Type::Error(self.core.report(diag)),
            },
          )
        }
      }
      ExprKind::Adt(adt_id, variant_id, generics, fields) => {
        let type_params = self.check_generics(generics, self.chart.adts[*adt_id].generics, true);
        let (forms, types) =
          fields.iter_mut().map(|x| self.check_expr(x)).collect::<(Vec<_>, Vec<_>)>();
        let adt = &self.chart.adts[*adt_id];
        let field_types = self.types.adt_types[*adt_id][*variant_id]
          .iter()
          .map(|t| t.instantiate(&type_params))
          .collect::<Vec<_>>();
        let form = if adt.is_struct { self.tuple_form(span, &forms) } else { Form::Value };
        for (((expr_form, expr), mut expr_type), mut field_type) in
          forms.into_iter().zip(fields).zip(types).zip(field_types)
        {
          self.coerce_expr(expr, expr_form, form);
          if !self.unifier.unify(&mut expr_type, &mut field_type) {
            self.core.report(Diag::ExpectedTypeFound {
              span: expr.span,
              expected: self.display_type(&field_type),
              found: self.display_type(&expr_type),
            });
          }
        }
        (form, Type::Adt(*adt_id, type_params))
      }
    }
  }

  fn tuple_form(&mut self, span: Span, forms: &[Form]) -> Form {
    let space = forms.iter().any(|&x| x == Form::Space);
    let value = forms.iter().any(|&x| x == Form::Value);
    if !space && !value {
      Form::Place
    } else if space && !value {
      Form::Space
    } else if value && !space {
      Form::Value
    } else {
      Form::Error(self.core.report(Diag::InconsistentTupleForm { span }))
    }
  }

  fn _check_expr_value(&mut self, expr: &mut Expr<'core>) -> Type<'core> {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![error || place || space || synthetic] | ExprKind::Path(_) => unreachable!(),
      ExprKind::DynFn(x) => self.dyn_fns[x].clone(),
      ExprKind::Def(id, args) => {
        let type_params = self.check_generics(args, self.chart.values[*id].generics, true);
        self.types.value_types[*id].instantiate(&type_params)
      }
      ExprKind::Do(label, block) => {
        let mut ty = self.unifier.new_var(span);
        *self.labels.get_or_extend(label.as_id()) = Some(ty.clone());
        let mut got = self.check_block(block);
        if !self.unifier.unify(&mut ty, &mut got) {
          self.core.report(Diag::ExpectedTypeFound {
            span: block.span,
            expected: self.display_type(&ty),
            found: self.display_type(&got),
          });
        }
        ty
      }
      ExprKind::Assign(_, space, value) => {
        let mut ty = self.check_expr_form(space, Form::Space);
        self.check_expr_form_type(value, Form::Value, &mut ty);
        Type::NIL
      }
      ExprKind::Match(scrutinee, arms) => {
        let mut scrutinee = self.check_expr_form(scrutinee, Form::Value);
        let mut result = self.unifier.new_var(span);
        for (pat, block) in arms {
          self.check_pat_type(pat, Form::Value, true, &mut scrutinee);
          self.check_block_type(block, &mut result);
        }
        result
      }
      ExprKind::If(arms, leg) => {
        let mut result = if leg.is_some() { self.unifier.new_var(span) } else { Type::NIL };
        for (cond, block) in arms {
          self.check_expr_form_type(cond, Form::Value, &mut Type::Bool);
          self.check_block_type(block, &mut result);
        }
        if let Some(leg) = leg {
          self.check_block_type(leg, &mut result);
        }
        result
      }
      ExprKind::While(label, cond, block) => {
        *self.labels.get_or_extend(label.as_id()) = Some(Type::NIL);
        self.check_expr_form_type(cond, Form::Value, &mut Type::Bool);
        self.check_block(block);
        Type::NIL
      }
      ExprKind::Loop(label, block) => {
        let result = self.unifier.new_var(span);
        *self.labels.get_or_extend(label.as_id()) = Some(result.clone());
        self.check_block(block);
        result
      }
      ExprKind::Fn(args, ret, body) => Type::Fn(
        args.iter_mut().map(|pat| self.check_pat(pat, Form::Value, false)).collect(),
        Box::new({
          let mut ret = match ret {
            Some(Some(t)) => self.hydrate_type(t, true),
            Some(None) => Type::NIL,
            None => self.unifier.new_var(span),
          };
          let old = self.return_ty.replace(ret.clone());
          self.check_block_type(body, &mut ret);
          self.return_ty = old;
          ret
        }),
      ),
      ExprKind::Break(label, e) => {
        if let Label::Error(_) = label {
          if let Some(e) = e {
            self.check_expr_form(e, Form::Value);
          }
        } else {
          let mut ty = self.labels[label.as_id()].clone().unwrap();
          if let Some(e) = e {
            self.check_expr_form_type(e, Form::Value, &mut ty);
          } else if !self.unifier.unify(&mut ty, &mut { Type::NIL }) {
            self.core.report(Diag::MissingBreakExpr { span, ty: self.display_type(&ty) });
          }
        }
        self.unifier.new_var(span)
      }
      ExprKind::Return(e) => {
        if let Some(ty) = &self.return_ty {
          let mut ty = ty.clone();
          if let Some(e) = e {
            self.check_expr_form_type(e, Form::Value, &mut ty);
          } else if !self.unifier.unify(&mut ty, &mut { Type::NIL }) {
            self.core.report(Diag::MissingReturnExpr { span, ty: self.display_type(&ty) });
          }
        } else {
          self.core.report(Diag::NoReturn { span });
        }
        self.unifier.new_var(span)
      }
      ExprKind::Continue(_) => self.unifier.new_var(span),
      ExprKind::Ref(expr, _) => {
        let ty = self.check_expr_form(expr, Form::Place);
        Type::Ref(Box::new(ty))
      }
      ExprKind::Move(expr, _) => self.check_expr_form(expr, Form::Place),
      ExprKind::List(els) => {
        let mut item = self.unifier.new_var(span);
        for el in els {
          self.check_expr_form_type(el, Form::Value, &mut item);
        }
        if let Some(list) = self.chart.builtins.list {
          Type::Adt(list, vec![item])
        } else {
          Type::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "List" }))
        }
      }
      ExprKind::Method(receiver, name, generics, args) => {
        let (desugared, ty) = self.check_method(span, receiver, *name, generics, args);
        expr.kind = desugared;
        ty
      }
      ExprKind::Call(func, args) => self.check_call(func, args, span),
      ExprKind::Bool(_) => Type::Bool,
      ExprKind::Not(expr) => {
        self.check_expr_form_type(expr, Form::Value, &mut Type::Bool);
        Type::Bool
      }
      ExprKind::Is(expr, pat) => {
        let mut ty = self.check_expr_form(expr, Form::Value);
        self.check_pat_type(pat, Form::Value, true, &mut ty);
        Type::Bool
      }
      ExprKind::LogicalOp(_, a, b) => {
        self.check_expr_form_type(a, Form::Value, &mut Type::Bool);
        self.check_expr_form_type(b, Form::Value, &mut Type::Bool);
        Type::Bool
      }
      ExprKind::Neg(expr) => {
        let ty = self.check_expr_form(expr, Form::Value);
        self.check_bin_op(span, BinaryOp::Sub, false, Type::N32, ty)
      }
      ExprKind::BinaryOp(op, a, b) => {
        let a = self.check_expr_form(a, Form::Value);
        let b = self.check_expr_form(b, Form::Value);
        self.check_bin_op(span, *op, false, a, b)
      }
      ExprKind::BinaryOpAssign(op, a, b) => {
        let a = self.check_expr_form(a, Form::Place);
        let b = self.check_expr_form(b, Form::Value);
        self.check_bin_op(span, *op, true, a, b);
        Type::NIL
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut lhs = self.check_expr_form(init, Form::Value);
        self.unifier.concretize(&mut lhs);
        for (_, next) in cmps {
          let mut rhs = self.check_expr_form(next, Form::Value);
          self.unifier.concretize(&mut rhs);
          if !matches!(
            (&lhs, &rhs),
            (Type::N32, Type::N32)
              | (Type::F32, Type::F32)
              | (Type::Char, Type::Char)
              | (Type::Bool, Type::Bool)
              | (Type::Error(_), _)
              | (_, Type::Error(_))
          ) {
            self.core.report(Diag::CannotCompare {
              span,
              lhs: self.display_type(&lhs),
              rhs: self.display_type(&rhs),
            });
          }
          lhs = rhs;
        }
        Type::Bool
      }
      ExprKind::N32(_) => Type::N32,
      ExprKind::Char(_) => Type::Char,
      ExprKind::F32(_) => Type::F32,
      ExprKind::String(_, rest) => {
        let mut string_ty =
          self.chart.builtins.string.map(|d| Type::Adt(d, Vec::new())).unwrap_or_else(|| {
            Type::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "String" }))
          });
        if let (Some(to_string_trait), Some(to_string_fn)) =
          (self.chart.builtins.to_string_trait, self.chart.builtins.to_string_fn)
        {
          for (expr, _) in rest {
            let span = expr.span;
            let ty = self.check_expr_form(expr, Form::Value);
            let impl_ = self.find_impl(span, &Type::Trait(to_string_trait, vec![ty]));
            *expr = Expr {
              span,
              kind: ExprKind::Call(
                Box::new(Expr {
                  span,
                  kind: ExprKind::Def(
                    to_string_fn,
                    Generics { span, types: vec![], impls: vec![impl_] },
                  ),
                }),
                vec![take(expr)],
              ),
            }
          }
        } else {
          for (expr, _) in rest {
            self.check_expr_form_type(expr, Form::Value, &mut string_ty);
          }
        }
        string_ty
      }
    }
  }

  fn check_call(
    &mut self,
    func: &mut Box<Expr<'core>>,
    args: &mut Vec<Expr<'core>>,
    span: Span,
  ) -> Type<'core> {
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
        Type::Error(self.core.report(e))
      }
    }
  }

  fn fn_sig(
    &mut self,
    span: Span,
    mut ty: Type<'core>,
    args: usize,
  ) -> Result<(Vec<Type<'core>>, Type<'core>), Diag<'core>> {
    self.unifier.concretize(&mut ty);
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

  fn check_bin_op(
    &mut self,
    span: Span,
    op: BinaryOp,
    assign: bool,
    mut a: Type<'core>,
    mut b: Type<'core>,
  ) -> Type<'core> {
    match op {
      BinaryOp::Concat => {
        if self.chart.builtins.concat.is_none() {
          return Type::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "concat" }));
        }
        self.unifier.concretize(&mut a);
        self.unifier.concretize(&mut b);
        if self.unifier.unify(&mut a, &mut b) {
          if let Type::Adt(n, _) = a {
            if Some(n) == self.chart.builtins.list || Some(n) == self.chart.builtins.string {
              return a;
            }
          }
        }
      }
      _ => {
        if let Ok(ty) = self._check_bin_op(op, assign, &mut a, &mut b, false, false) {
          return ty;
        }
      }
    }
    let diag =
      Diag::BadBinOp { span, op, assign, lhs: self.display_type(&a), rhs: self.display_type(&b) };
    Type::Error(self.core.report(diag))
  }

  fn _check_bin_op(
    &mut self,
    op: BinaryOp,
    assign: bool,
    mut a: &mut Type<'core>,
    mut b: &mut Type<'core>,
    i: bool,
    j: bool,
  ) -> Result<Type<'core>, ()> {
    self.unifier.concretize(a);
    self.unifier.concretize(b);
    match (op, &mut a, &mut b) {
      (_, Type::Error(e), _) | (_, _, Type::Error(e)) => Ok(Type::Error(*e)),
      (_, Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => a
        .iter_mut()
        .zip(b)
        .map(|(a, b)| self._check_bin_op(op, assign, a, b, i, j))
        .collect::<Result<_, _>>()
        .map(Type::Tuple),
      (_, Type::Tuple(a), b @ (Type::N32 | Type::F32)) => a
        .iter_mut()
        .map(|a| self._check_bin_op(op, assign, a, b, i, j))
        .collect::<Result<_, _>>()
        .map(Type::Tuple),
      (_, a @ (Type::N32 | Type::F32), Type::Tuple(b)) if !assign => b
        .iter_mut()
        .map(|b| self._check_bin_op(op, assign, a, b, i, j))
        .collect::<Result<_, _>>()
        .map(Type::Tuple),
      (_, Type::N32, Type::N32) if !i && !j => Ok(Type::N32),
      (BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor, Type::Bool, Type::Bool)
        if !i && !j =>
      {
        Ok(Type::Bool)
      }
      (BinaryOp::Add | BinaryOp::Sub, Type::Char, Type::N32) if !i && !j => Ok(Type::Char),
      (BinaryOp::Sub, Type::Char, Type::Char) if !i && !j => Ok(Type::N32),
      (
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem,
        a @ (Type::N32 | Type::F32),
        Type::N32 | Type::F32,
      ) if !i && !j && (!assign || matches!(a, Type::F32)) => Ok(Type::F32),
      (_, Type::Inverse(a), b) => self._check_bin_op(op, assign, a, b, !i, j),
      (_, a, Type::Inverse(b)) => self._check_bin_op(op, assign, a, b, i, !j),
      _ => Err(()),
    }
  }
}
