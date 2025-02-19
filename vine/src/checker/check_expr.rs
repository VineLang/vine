use std::{collections::BTreeMap, mem::take};

use crate::{
  ast::{Expr, ExprKind, Generics, ImplKind, Label, Span},
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
      ExprKind::Not(inner) => {
        let ty = self.check_expr_form(inner, Form::Value);
        let Some(op_fn) = self.chart.builtins.not else {
          return Type::Error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.unifier.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[op_fn].generics,
          true,
          Some(vec![ty, return_ty.clone()]),
        );
        if let ImplKind::Def(id, _) = generics.impls[0].kind {
          if self.chart.builtins.bool_not == Some(id) {
            return return_ty;
          }
        }
        expr.kind = ExprKind::Call(
          Box::new(Expr { span, kind: ExprKind::Def(op_fn, generics) }),
          vec![take(inner)],
        );
        return_ty
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
      ExprKind::Neg(inner) => {
        let ty = self.check_expr_form(inner, Form::Value);
        let Some(op_fn) = self.chart.builtins.neg else {
          return Type::Error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.unifier.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[op_fn].generics,
          true,
          Some(vec![ty, return_ty.clone()]),
        );
        expr.kind = ExprKind::Call(
          Box::new(Expr { span, kind: ExprKind::Def(op_fn, generics) }),
          vec![take(inner)],
        );
        return_ty
      }
      ExprKind::BinaryOp(op, lhs, rhs) => {
        let lhs_ty = self.check_expr_form(lhs, Form::Value);
        let rhs_ty = self.check_expr_form(rhs, Form::Value);
        let Some(&Some(op_fn)) = self.chart.builtins.binary_ops.get(op) else {
          return Type::Error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.unifier.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[op_fn].generics,
          true,
          Some(vec![lhs_ty, rhs_ty, return_ty.clone()]),
        );
        expr.kind = ExprKind::Call(
          Box::new(Expr { span, kind: ExprKind::Def(op_fn, generics) }),
          vec![take(lhs), take(rhs)],
        );
        return_ty
      }
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        let lhs_ty = self.check_expr_form(lhs, Form::Place);
        let rhs_ty = self.check_expr_form(rhs, Form::Value);
        let Some(&Some(op_fn)) = self.chart.builtins.binary_ops.get(op) else {
          return Type::Error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[op_fn].generics,
          true,
          Some(vec![lhs_ty.clone(), rhs_ty, lhs_ty]),
        );
        expr.kind = ExprKind::CallAssign(
          Box::new(Expr { span, kind: ExprKind::Def(op_fn, generics) }),
          take(lhs),
          take(rhs),
        );
        Type::NIL
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut ty = self.check_expr_pseudo_place(init);
        let mut err = false;
        for (_, expr) in cmps.iter_mut() {
          let mut other_ty = self.check_expr_pseudo_place(expr);
          if !self.unifier.unify(&mut ty, &mut other_ty) {
            self.core.report(Diag::CannotCompare {
              span,
              lhs: self.display_type(&ty),
              rhs: self.display_type(&other_ty),
            });
            err = true;
            ty = other_ty;
          }
        }
        if !err {
          let cmps = cmps.drain(..).map(|(op, expr)| {
            (
              if let Some(&Some(op_fn)) = self.chart.builtins.comparison_ops.get(&op) {
                let mut generics = Generics { span, ..Default::default() };
                self._check_generics(
                  &mut generics,
                  self.chart.values[op_fn].generics,
                  true,
                  Some(vec![ty.clone()]),
                );
                Expr { span, kind: ExprKind::Def(op_fn, generics) }
              } else {
                Expr {
                  span,
                  kind: ExprKind::Error(self.core.report(Diag::MissingOperatorBuiltin { span })),
                }
              },
              expr,
            )
          });
          expr.kind = ExprKind::CallCompare(take(init), cmps.collect());
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
        if let Some(to_string) = self.chart.builtins.to_string {
          for (expr, _) in rest {
            let span = expr.span;
            let ty = self.check_expr_form(expr, Form::Value);
            let mut generics = Generics { span, ..Default::default() };
            self._check_generics(
              &mut generics,
              self.chart.values[to_string].generics,
              true,
              Some(vec![ty]),
            );
            expr.kind = ExprKind::Call(
              Box::new(Expr { span, kind: ExprKind::Def(to_string, generics) }),
              vec![take(expr)],
            );
          }
        } else {
          for (expr, _) in rest {
            self.check_expr_form_type(expr, Form::Value, &mut string_ty);
          }
        }
        string_ty
      }
      ExprKind::InlineIvy(binds, ty, _, _) => {
        for (_, value, expr) in binds {
          self.check_expr_form(expr, if *value { Form::Value } else { Form::Space });
        }
        self.hydrate_type(ty, true)
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

  fn check_expr_pseudo_place(&mut self, expr: &mut Expr<'core>) -> Type<'core> {
    let span = expr.span;
    let (form, ty) = self.check_expr(expr);
    if form == Form::Value {
      expr.wrap(|e| ExprKind::Place(e, Box::new(Expr { span, kind: ExprKind::Hole })));
    } else {
      self.coerce_expr(expr, form, Form::Place);
    }
    ty
  }
}
