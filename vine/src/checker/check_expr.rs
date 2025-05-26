use std::{collections::BTreeMap, mem::take};

use crate::{
  ast::{Expr, ExprKind, Generics, Ident, ImplKind, Label, Span},
  chart::{StructId, TypeDefId},
  checker::{Checker, Form, Type},
  diag::Diag,
  types::{Inverted, TypeKind},
};

mod check_method;
mod coerce_expr;

impl<'core> Checker<'core, '_> {
  pub(super) fn check_expr_form_type(&mut self, expr: &mut Expr<'core>, form: Form, ty: Type) {
    let found = self.check_expr_form(expr, form);
    if self.types.unify(found, ty).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.types.show(self.chart, ty),
        found: self.types.show(self.chart, found),
      });
    }
  }

  pub(super) fn check_expr_form(&mut self, expr: &mut Expr<'core>, form: Form) -> Type {
    let (found, ty) = self.check_expr(expr);
    self.coerce_expr(expr, found, form);
    ty
  }

  pub(super) fn check_expr(&mut self, expr: &mut Expr<'core>) -> (Form, Type) {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![synthetic] => unreachable!(),
      ExprKind![!error && !space && !place && !synthetic] => {
        (Form::Value, self._check_expr_value(expr))
      }
      ExprKind::Paren(e) => self.check_expr(e),
      ExprKind::Error(e) => (Form::Error(*e), self.types.error(*e)),
      ExprKind::Hole => (Form::Space, self.types.new_var(span)),
      ExprKind::Local(l) => {
        (Form::Place, *self.locals.entry(*l).or_insert_with(|| self.types.new_var(span)))
      }
      ExprKind::Deref(e, _) => {
        let ty = self.types.new_var(span);
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        self.check_expr_form_type(e, Form::Value, ref_ty);
        (Form::Place, ty)
      }
      ExprKind::Inverse(expr, _) => {
        let (form, ty) = self.check_expr(expr);
        (form.inverse(), ty.inverse())
      }
      ExprKind::Place(v, s) => {
        let v = self.check_expr_form(v, Form::Value);
        let s = self.check_expr_form(s, Form::Space);
        let ty = if self.types.unify(v, s).is_failure() {
          self.types.error(self.core.report(Diag::MismatchedValueSpaceTypes {
            span,
            value: self.types.show(self.chart, v),
            space: self.types.show(self.chart, s),
          }))
        } else {
          v
        };
        (Form::Place, ty)
      }
      ExprKind::TupleField(e, i, l) => {
        let (form, ty) = self.check_expr(e);

        if form == Form::Space {
          let e = self.core.report(Diag::SpaceField { span });
          return (Form::Error(e), self.types.error(e));
        }

        let (field_ty, len) = self.tuple_field(span, ty, *i).unwrap_or_else(|| {
          (
            self.types.error(self.core.report(Diag::MissingTupleField {
              span,
              ty: self.types.show(self.chart, ty),
              i: *i,
            })),
            0,
          )
        });
        *l = Some(len);

        (form, field_ty)
      }
      ExprKind::ObjectField(e, k) => {
        let (form, ty) = self.check_expr(e);

        if form == Form::Space {
          let e = self.core.report(Diag::SpaceField { span });
          return (Form::Error(e), self.types.error(e));
        }

        let (field_ty, i, len) = self.object_field(span, ty, k.ident).unwrap_or_else(|| {
          let ty = self.types.error(self.core.report(Diag::MissingObjectField {
            span: k.span,
            ty: self.types.show(self.chart, ty),
            key: k.ident,
          }));
          (ty, 0, 0)
        });

        expr.kind = ExprKind::TupleField(take(e), i, Some(len));

        (form, field_ty)
      }
      ExprKind::Tuple(v) => {
        if v.is_empty() {
          (Form::Place, self.types.nil())
        } else {
          let (forms, types) =
            v.iter_mut().map(|x| self.check_expr(x)).collect::<(Vec<_>, Vec<_>)>();
          let form = self.tuple_form(span, &forms);
          for (f, e) in forms.into_iter().zip(v) {
            self.coerce_expr(e, f, form);
          }
          (form, self.types.new(TypeKind::Tuple(types)))
        }
      }
      ExprKind::Object(entries) => {
        if entries.is_empty() {
          (Form::Place, self.types.new(TypeKind::Object(BTreeMap::new())))
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
          (form, ty)
        }
      }
      ExprKind::Struct(struct_id, generics, data) => {
        let type_params =
          self.check_generics(generics, self.chart.structs[*struct_id].generics, true);
        let data_ty =
          self.types.import(&self.sigs.structs[*struct_id], Some(&type_params), |t, sig| {
            t.transfer(sig.data)
          });
        let (form, ty) = self.check_expr(data);
        if self.types.unify(ty, data_ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span: expr.span,
            expected: self.types.show(self.chart, data_ty),
            found: self.types.show(self.chart, ty),
          });
        }
        (form, self.types.new(TypeKind::Struct(*struct_id, type_params)))
      }
      ExprKind::Unwrap(inner) => {
        let (form, ty) = self.check_expr(inner);

        let data_ty = match self.types.force_kind(self.core, ty) {
          (inv, TypeKind::Struct(struct_id, type_params)) => {
            let struct_id = *struct_id;
            let type_params = &type_params.clone();
            self.get_struct_data(span, ty, struct_id, type_params).invert_if(inv)
          }
          _ => self.types.error(self.core.report(Diag::UnwrapNonStruct { span })),
        };

        (form, data_ty)
      }
    }
  }

  fn get_struct_data(
    &mut self,
    span: Span,
    ty: Type,
    struct_id: StructId,
    type_params: &[Type],
  ) -> Type {
    let vis = self.chart.structs[struct_id].data_vis;
    if !self.chart.visible(vis, self.cur_def) {
      self.core.report(Diag::StructDataInvisible {
        span,
        ty: self.types.show(self.chart, ty),
        vis: self.chart.defs[vis].path,
      });
    }
    self
      .types
      .import(&self.sigs.structs[struct_id], Some(type_params), |t, sig| t.transfer(sig.data))
  }

  fn tuple_field(&mut self, span: Span, ty: Type, index: usize) -> Option<(Type, usize)> {
    match self.types.force_kind(self.core, ty) {
      (inv, TypeKind::Tuple(tuple)) => Some((tuple.get(index)?.invert_if(inv), tuple.len())),
      (inv, TypeKind::Struct(struct_id, type_params)) => {
        let struct_id = *struct_id;
        let type_params = &type_params.clone();
        let data_ty = self.get_struct_data(span, ty, struct_id, type_params);
        self.tuple_field(span, data_ty.invert_if(inv), index)
      }
      (_, TypeKind::Error(_)) => Some((ty, 0)),
      _ => None,
    }
  }

  fn object_field(
    &mut self,
    span: Span,
    ty: Type,
    key: Ident<'core>,
  ) -> Option<(Type, usize, usize)> {
    match self.types.force_kind(self.core, ty) {
      (inv, TypeKind::Object(entries)) => entries
        .iter()
        .enumerate()
        .find(|&(_, (&k, _))| k == key)
        .map(|(i, (_, t))| (t.invert_if(inv), i, entries.len())),
      (inv, TypeKind::Struct(struct_id, type_params)) => {
        let struct_id = *struct_id;
        let type_params = &type_params.clone();
        let data_ty = self.get_struct_data(span, ty, struct_id, type_params);
        self.object_field(span, data_ty.invert_if(inv), key)
      }
      (_, TypeKind::Error(_)) => Some((ty, 0, 0)),
      _ => None,
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

  fn _check_expr_value(&mut self, expr: &mut Expr<'core>) -> Type {
    let nil = self.types.nil();
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![error || place || space || synthetic] | ExprKind::Path(..) => unreachable!(),
      ExprKind::LetFn(x) => self.let_fns[x],
      ExprKind::Def(id, args) => {
        let type_params = self.check_generics(args, self.chart.values[*id].generics, true);
        self.types.import(&self.sigs.values[*id], Some(&type_params), |t, sig| t.transfer(sig.ty))
      }
      ExprKind::Do(label, block) => {
        let ty = self.types.new_var(span);
        *self.labels.get_or_extend(label.as_id()) = Some(ty);
        let got = self.check_block(block);
        if self.types.unify(ty, got).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span: block.span,
            expected: self.types.show(self.chart, ty),
            found: self.types.show(self.chart, got),
          });
        }
        ty
      }
      ExprKind::Assign(_, space, value) => {
        let ty = self.check_expr_form(space, Form::Space);
        self.check_expr_form_type(value, Form::Value, ty);
        self.types.nil()
      }
      ExprKind::Match(scrutinee, arms) => {
        let scrutinee = self.check_expr_form(scrutinee, Form::Value);
        let result = self.types.new_var(span);
        for (pat, block) in arms {
          self.check_pat_type(pat, Form::Value, true, scrutinee);
          self.check_block_type(block, result);
        }
        result
      }
      ExprKind::If(arms, leg) => {
        let bool = self.bool(span);
        let result = if leg.is_some() { self.types.new_var(span) } else { self.types.nil() };
        for (cond, block) in arms {
          self.check_expr_form_type(cond, Form::Value, bool);
          self.check_block_type(block, result);
        }
        if let Some(leg) = leg {
          self.check_block_type(leg, result);
        }
        result
      }
      ExprKind::While(label, cond, block) => {
        let bool = self.bool(span);
        *self.labels.get_or_extend(label.as_id()) = Some(self.types.nil());
        self.check_expr_form_type(cond, Form::Value, bool);
        self.check_block(block);
        self.types.nil()
      }
      ExprKind::Loop(label, block) => {
        let result = self.types.new_var(span);
        *self.labels.get_or_extend(label.as_id()) = Some(result);
        self.check_block(block);
        result
      }
      ExprKind::Fn(params, ret, body) => {
        let params = params.iter_mut().map(|pat| self.check_pat(pat, Form::Value, false)).collect();
        let ret = match ret {
          Some(Some(t)) => self.hydrate_type(t, true),
          Some(None) => self.types.nil(),
          None => self.types.new_var(span),
        };
        let old = self.return_ty.replace(ret);
        self.check_block_type(body, ret);
        self.return_ty = old;
        self.types.new(TypeKind::Fn(params, ret))
      }
      ExprKind::Break(label, e) => {
        if let Label::Error(_) = label {
          if let Some(e) = e {
            self.check_expr_form(e, Form::Value);
          }
        } else {
          let ty = self.labels[label.as_id()].unwrap();
          if let Some(e) = e {
            self.check_expr_form_type(e, Form::Value, ty);
          } else if self.types.unify(ty, nil).is_failure() {
            self.core.report(Diag::MissingBreakExpr { span, ty: self.types.show(self.chart, ty) });
          }
        }
        self.types.new_var(span)
      }
      ExprKind::Return(e) => {
        if let Some(ty) = &self.return_ty {
          let ty = *ty;
          if let Some(e) = e {
            self.check_expr_form_type(e, Form::Value, ty);
          } else if self.types.unify(ty, nil).is_failure() {
            self.core.report(Diag::MissingReturnExpr { span, ty: self.types.show(self.chart, ty) });
          }
        } else {
          self.core.report(Diag::NoReturn { span });
        }
        self.types.new_var(span)
      }
      ExprKind::Continue(_) => self.types.new_var(span),
      ExprKind::Ref(expr, _) => {
        let ty = self.check_expr_form(expr, Form::Place);
        self.types.new(TypeKind::Ref(ty))
      }
      ExprKind::Move(expr, _) => self.check_expr_form(expr, Form::Place),
      ExprKind::List(els) => {
        let item = self.types.new_var(span);
        for el in els {
          self.check_expr_form_type(el, Form::Value, item);
        }
        if let Some(list) = self.chart.builtins.list {
          self.types.new(TypeKind::Struct(list, vec![item]))
        } else {
          self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: "List" }))
        }
      }
      ExprKind::Method(receiver, name, generics, args) => {
        let (desugared, ty) = self.check_method(span, receiver, *name, generics, args);
        expr.kind = desugared;
        ty
      }
      ExprKind::Call(func, args) => self.check_call(func, args, span),
      ExprKind::Bool(_) => self.bool(span),
      ExprKind::Not(inner) => {
        let ty = self.check_expr_form(inner, Form::Value);
        let Some(op_fn) = self.chart.builtins.not else {
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.types.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[op_fn].generics,
          true,
          Some(vec![ty, return_ty]),
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
        let ty = self.check_expr_form(expr, Form::Value);
        self.check_pat_type(pat, Form::Value, true, ty);
        self.bool(span)
      }
      ExprKind::LogicalOp(_, a, b) => {
        let bool = self.bool(span);
        self.check_expr_form_type(a, Form::Value, bool);
        self.check_expr_form_type(b, Form::Value, bool);
        bool
      }
      ExprKind::Neg(inner) => {
        let ty = self.check_expr_form(inner, Form::Value);
        let Some(op_fn) = self.chart.builtins.neg else {
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.types.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[op_fn].generics,
          true,
          Some(vec![ty, return_ty]),
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
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.types.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[op_fn].generics,
          true,
          Some(vec![lhs_ty, rhs_ty, return_ty]),
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
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[op_fn].generics,
          true,
          Some(vec![lhs_ty, rhs_ty, lhs_ty]),
        );
        expr.kind = ExprKind::CallAssign(
          Box::new(Expr { span, kind: ExprKind::Def(op_fn, generics) }),
          take(lhs),
          take(rhs),
        );
        self.types.nil()
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut ty = self.check_expr_pseudo_place(init);
        let mut err = false;
        for (_, expr) in cmps.iter_mut() {
          let other_ty = self.check_expr_pseudo_place(expr);
          if self.types.unify(ty, other_ty).is_failure() {
            self.core.report(Diag::CannotCompare {
              span,
              lhs: self.types.show(self.chart, ty),
              rhs: self.types.show(self.chart, other_ty),
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
                  Some(vec![ty]),
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
        self.bool(span)
      }
      ExprKind::Cast(cur, to, _) => {
        let cur_ty = self.check_expr_form(cur, Form::Value);
        let to_ty = self.hydrate_type(to, true);
        let Some(cast_fn) = self.chart.builtins.cast else {
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let mut generics = Generics { span, ..Default::default() };
        self._check_generics(
          &mut generics,
          self.chart.values[cast_fn].generics,
          true,
          Some(vec![cur_ty, to_ty]),
        );
        expr.kind = ExprKind::Call(
          Box::new(Expr { span, kind: ExprKind::Def(cast_fn, generics) }),
          vec![take(cur)],
        );
        to_ty
      }
      ExprKind::N32(_) => self.builtin_ty(span, "N32", self.chart.builtins.n32),
      ExprKind::I32(_) => self.builtin_ty(span, "I32", self.chart.builtins.i32),
      ExprKind::Char(_) => self.builtin_ty(span, "Char", self.chart.builtins.char),
      ExprKind::F32(_) => self.builtin_ty(span, "F32", self.chart.builtins.f32),
      ExprKind::String(_, rest) => {
        let string_ty = if let Some(string) = self.chart.builtins.string {
          self.types.new(TypeKind::Struct(string, Vec::new()))
        } else {
          self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: "String" }))
        };
        if let Some(cast) = self.chart.builtins.cast {
          for (expr, _) in rest {
            let span = expr.span;
            let ty = self.check_expr_form(expr, Form::Value);
            let mut generics = Generics { span, ..Default::default() };
            self._check_generics(
              &mut generics,
              self.chart.values[cast].generics,
              true,
              Some(vec![ty, string_ty]),
            );
            expr.kind = ExprKind::Call(
              Box::new(Expr { span, kind: ExprKind::Def(cast, generics) }),
              vec![take(expr)],
            );
          }
        } else {
          for (expr, _) in rest {
            self.check_expr_form_type(expr, Form::Value, string_ty);
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
      ExprKind::Enum(enum_id, variant_id, generics, data) => {
        let type_params = self.check_generics(generics, self.chart.enums[*enum_id].generics, true);
        let data_ty =
          self.types.import(&self.sigs.enums[*enum_id], Some(&type_params), |t, sig| {
            Some(t.transfer(sig.variant_data[*variant_id]?))
          });
        match (data, data_ty) {
          (None, None) => {}
          (Some(data), Some(data_ty)) => self.check_expr_form_type(data, Form::Value, data_ty),
          (None, Some(_)) => {
            self.core.report(Diag::ExpectedDataExpr { span });
          }
          (Some(_), None) => {
            self.core.report(Diag::EnumVariantNoData { span });
          }
        }
        self.types.new(TypeKind::Enum(*enum_id, type_params))
      }
    }
  }

  fn check_call(
    &mut self,
    func: &mut Box<Expr<'core>>,
    args: &mut Vec<Expr<'core>>,
    span: Span,
  ) -> Type {
    let ty = self.check_expr_form(func, Form::Value);
    match self.fn_sig(span, ty, args.len()) {
      Ok((params, ret)) => {
        for (ty, arg) in params.into_iter().zip(args) {
          self.check_expr_form_type(arg, Form::Value, ty);
        }
        ret
      }
      Err(e) => {
        for arg in args {
          self.check_expr_form(arg, Form::Value);
        }
        self.types.error(self.core.report(e))
      }
    }
  }

  fn fn_sig(
    &mut self,
    span: Span,
    ty: Type,
    args: usize,
  ) -> Result<(Vec<Type>, Type), Diag<'core>> {
    match self.types.force_kind(self.core, ty) {
      (Inverted(false), TypeKind::Fn(params, ret)) => {
        if params.len() != args {
          Err(Diag::BadArgCount {
            span,
            expected: params.len(),
            got: args,
            ty: self.types.show(self.chart, ty),
          })
        } else {
          Ok((params.clone(), *ret))
        }
      }
      (_, TypeKind::Error(e)) => Err(*e)?,
      _ => Err(Diag::NonFunctionCall { span, ty: self.types.show(self.chart, ty) }),
    }
  }

  fn check_expr_pseudo_place(&mut self, expr: &mut Expr<'core>) -> Type {
    let span = expr.span;
    let (form, ty) = self.check_expr(expr);
    if form == Form::Value {
      expr.wrap(|e| ExprKind::Place(e, Box::new(Expr { span, kind: ExprKind::Hole })));
    } else {
      self.coerce_expr(expr, form, Form::Place);
    }
    ty
  }

  fn builtin_ty(&mut self, span: Span, name: &'static str, builtin: Option<TypeDefId>) -> Type {
    if let Some(id) = builtin {
      self.types.new(TypeKind::Opaque(id, vec![]))
    } else {
      self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: name }))
    }
  }

  fn bool(&mut self, span: Span) -> Type {
    self.builtin_ty(span, "Bool", self.chart.builtins.bool)
  }
}
