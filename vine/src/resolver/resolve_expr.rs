use std::{collections::BTreeMap, mem::take};

use crate::{
  ast::{Expr, ExprKind, Generics, Ident, ImplKind, Label, LogicalOp, Span},
  chart::{DefValueKind, OpaqueTypeId, StructId},
  diag::Diag,
  resolver::{Binding, Form, Resolver, Type},
  types::{Inverted, TypeKind},
};

mod coerce_expr;
mod resolve_method;

impl<'core> Resolver<'core, '_> {
  pub(super) fn resolve_expr_form_type(&mut self, expr: &mut Expr<'core>, form: Form, ty: Type) {
    let found = self.resolve_expr_form(expr, form);
    if self.types.unify(found, ty).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.types.show(self.chart, ty),
        found: self.types.show(self.chart, found),
      });
    }
  }

  pub(super) fn resolve_expr_form(&mut self, expr: &mut Expr<'core>, form: Form) -> Type {
    let (found, ty) = self.resolve_expr(expr);
    self.coerce_expr(expr, found, form);
    ty
  }

  pub(super) fn resolve_expr(&mut self, expr: &mut Expr<'core>) -> (Form, Type) {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![synthetic] => unreachable!(),
      ExprKind![!error && !space && !place && !synthetic] => {
        (Form::Value, self._resolve_expr_value(expr))
      }
      ExprKind::Paren(e) => self.resolve_expr(e),
      ExprKind::Error(e) => (Form::Error(*e), self.types.error(*e)),
      ExprKind::Hole => (Form::Space, self.types.new_var(span)),
      ExprKind::Deref(e, _) => {
        let ty = self.types.new_var(span);
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        self.resolve_expr_form_type(e, Form::Value, ref_ty);
        (Form::Place, ty)
      }
      ExprKind::Inverse(expr, _) => {
        let (form, ty) = self.resolve_expr(expr);
        (form.inverse(), ty.inverse())
      }
      ExprKind::Place(v, s) => {
        let v = self.resolve_expr_form(v, Form::Value);
        let s = self.resolve_expr_form(s, Form::Space);
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
        let (form, ty) = self.resolve_expr(e);

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
        let (form, ty) = self.resolve_expr(e);

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
            v.iter_mut().map(|x| self.resolve_expr(x)).collect::<(Vec<_>, Vec<_>)>();
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
            let (form, ty) = self_.resolve_expr(e);
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
      ExprKind::Unwrap(inner) => {
        let (form, ty) = self.resolve_expr(inner);

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
      ExprKind::Path(path, args) => {
        let mut args = args.take();
        let (form, ty) = 'inner: {
          if let Some(ident) = path.as_ident() {
            if let Some(bind) = self.scope.get(&ident).and_then(|x| x.last()) {
              break 'inner match bind.binding {
                Binding::Local(local, ty) => {
                  expr.kind = ExprKind::Local(local);
                  (Form::Place, ty)
                }
                Binding::LetFn(id, ty) => {
                  expr.kind = ExprKind::LetFn(id);
                  (Form::Value, ty)
                }
              };
            }
          }
          let mut generics = path.take_generics();
          match self.resolve_path_to(self.cur_def, path, "value", |d| d.value_kind) {
            Ok(DefValueKind::Struct(struct_id)) => {
              let type_params =
                self.resolve_generics(&mut generics, self.chart.structs[struct_id].generics, true);
              let data_ty =
                self.types.import(&self.sigs.structs[struct_id], Some(&type_params)).data;
              let ((form, ty), data) = if let Some(mut args) = args.take() {
                let mut data = if args.len() != 1 {
                  Expr { span, kind: ExprKind::Tuple(args) }
                } else {
                  args.pop().unwrap()
                };
                (self.resolve_expr(&mut data), data)
              } else {
                let err = self.core.report(Diag::ExpectedDataExpr { span });
                (
                  (Form::Error(err), self.types.error(err)),
                  Expr { span, kind: ExprKind::Error(err) },
                )
              };
              if self.types.unify(ty, data_ty).is_failure() {
                self.core.report(Diag::ExpectedTypeFound {
                  span: expr.span,
                  expected: self.types.show(self.chart, data_ty),
                  found: self.types.show(self.chart, ty),
                });
              }
              expr.kind = ExprKind::Struct(struct_id, generics, Box::new(data));
              (form, self.types.new(TypeKind::Struct(struct_id, type_params)))
            }
            Ok(DefValueKind::Enum(enum_id, variant_id)) => {
              let type_params =
                self.resolve_generics(&mut generics, self.chart.enums[enum_id].generics, true);
              let data_ty =
                self.types.import_with(&self.sigs.enums[enum_id], Some(&type_params), |t, sig| {
                  Some(t.transfer(&sig.variant_data[variant_id]?))
                });
              let mut data = args.take().map(|mut args| {
                if args.len() != 1 {
                  Expr { span, kind: ExprKind::Tuple(args) }
                } else {
                  args.pop().unwrap()
                }
              });
              match (&mut data, data_ty) {
                (None, None) => {}
                (Some(data), Some(data_ty)) => {
                  self.resolve_expr_form_type(data, Form::Value, data_ty)
                }
                (None, Some(_)) => {
                  self.core.report(Diag::ExpectedDataExpr { span });
                }
                (Some(_), None) => {
                  self.core.report(Diag::EnumVariantNoData { span });
                }
              }
              expr.kind = ExprKind::Enum(enum_id, variant_id, generics, data.map(Box::new));
              (Form::Value, self.types.new(TypeKind::Enum(enum_id, type_params)))
            }
            Ok(DefValueKind::Const(const_id)) => {
              let type_params =
                self.resolve_generics(&mut generics, self.chart.const_generics(const_id), true);
              expr.kind = ExprKind::ConstDef(const_id, generics);
              (Form::Value, self.types.import(self.sigs.const_sig(const_id), Some(&type_params)).ty)
            }
            Ok(DefValueKind::Fn(fn_id)) => {
              let type_params =
                self.resolve_generics(&mut generics, self.chart.fn_generics(fn_id), true);
              let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
              expr.kind = ExprKind::FnDef(fn_id, generics);
              (Form::Value, self.types.new(TypeKind::Fn(sig.params, sig.ret_ty)))
            }
            Err(diag) => {
              let err = self.core.report(diag);
              expr.kind = ExprKind::Error(err);
              (Form::Error(err), self.types.error(err))
            }
          }
        };
        if let Some(mut args) = args {
          self.coerce_expr(expr, form, Form::Value);
          let ty = self._resolve_call(span, ty, &mut args);
          *expr = Expr { span, kind: ExprKind::Call(Box::new(take(expr)), args) };
          (Form::Value, ty)
        } else {
          (form, ty)
        }
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
    self.types.import(&self.sigs.structs[struct_id], Some(type_params)).data
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

  fn _resolve_expr_value(&mut self, expr: &mut Expr<'core>) -> Type {
    let nil = self.types.nil();
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![error || place || space || synthetic] => unreachable!(),
      ExprKind::Do(label, block) => {
        let ty = self.types.new_var(span);
        self.bind_label(label, false, ty, |self_| self_.resolve_block_type(block, ty));
        ty
      }
      ExprKind::Assign(_, space, value) => {
        let ty = self.resolve_expr_form(space, Form::Space);
        self.resolve_expr_form_type(value, Form::Value, ty);
        self.types.nil()
      }
      ExprKind::Match(scrutinee, arms) => {
        let scrutinee = self.resolve_expr_form(scrutinee, Form::Value);
        let result = self.types.new_var(span);
        for (pat, block) in arms {
          self.enter_scope();
          self.resolve_pat_type(pat, Form::Value, true, scrutinee);
          self.resolve_block_type(block, result);
          self.exit_scope();
        }
        result
      }
      ExprKind::If(arms, leg) => {
        let result = if leg.is_some() { self.types.new_var(span) } else { self.types.nil() };
        for (cond, block) in arms {
          self.enter_scope();
          self.resolve_cond(cond);
          self.resolve_block_type(block, result);
          self.exit_scope();
        }
        if let Some(leg) = leg {
          self.resolve_block_type(leg, result);
        }
        result
      }
      ExprKind::While(label, cond, block) => {
        let nil = self.types.nil();
        self.bind_label(label, true, nil, |self_| {
          self_.enter_scope();
          self_.resolve_cond(cond);
          self_.resolve_block(block);
          self_.exit_scope();
        });
        nil
      }
      ExprKind::Loop(label, block) => {
        let result = self.types.new_var(span);
        self.bind_label(label, true, result, |self_| {
          self_.resolve_block(block);
        });
        result
      }
      ExprKind::Fn(params, ret, body) => {
        let old_labels = take(&mut self.labels);
        let old_loops = take(&mut self.loops);
        self.enter_scope();
        let params =
          params.iter_mut().map(|pat| self.resolve_pat(pat, Form::Value, false)).collect();
        let ret = match ret {
          Some(Some(t)) => self.resolve_type(t, true),
          Some(None) => self.types.nil(),
          None => self.types.new_var(span),
        };
        let old_return_ty = self.return_ty.replace(ret);
        self.resolve_block_type(body, ret);
        self.exit_scope();
        self.return_ty = old_return_ty;
        self.labels = old_labels;
        self.loops = old_loops;
        self.types.new(TypeKind::Fn(params, ret))
      }
      ExprKind::Break(label, value) => {
        let label_info = if let Label::Ident(Some(label)) = *label {
          self.labels.get(&label).copied().ok_or(Diag::UnboundLabel { span, label })
        } else {
          self.loops.last().copied().ok_or(Diag::NoLoopBreak { span })
        };
        match label_info {
          Ok(label_info) => {
            *label = Label::Resolved(label_info.id);
            if let Some(value) = value {
              self.resolve_expr_form_type(value, Form::Value, label_info.break_ty);
            } else if self.types.unify(label_info.break_ty, nil).is_failure() {
              self.core.report(Diag::MissingBreakExpr {
                span,
                ty: self.types.show(self.chart, label_info.break_ty),
              });
            }
          }
          Err(diag) => {
            *label = Label::Error(self.core.report(diag));
            if let Some(value) = value {
              self.resolve_expr_form(value, Form::Value);
            }
          }
        }
        self.types.new_var(span)
      }
      ExprKind::Return(e) => {
        if let Some(ty) = &self.return_ty {
          let ty = *ty;
          if let Some(e) = e {
            self.resolve_expr_form_type(e, Form::Value, ty);
          } else if self.types.unify(ty, nil).is_failure() {
            self.core.report(Diag::MissingReturnExpr { span, ty: self.types.show(self.chart, ty) });
          }
        } else {
          self.core.report(Diag::NoReturn { span });
        }
        self.types.new_var(span)
      }
      ExprKind::Continue(label) => {
        let info = if let Label::Ident(Some(label)) = *label {
          self.labels.get(&label).ok_or(Diag::UnboundLabel { span, label }).and_then(|&info| {
            if info.is_loop {
              Ok(info)
            } else {
              Err(Diag::NoContinueLabel { span, label })
            }
          })
        } else {
          self.loops.last().copied().ok_or(Diag::NoLoopContinue { span })
        };
        match info {
          Ok(info) => {
            *label = Label::Resolved(info.id);
          }
          Err(diag) => {
            *label = Label::Error(self.core.report(diag));
          }
        }
        self.types.new_var(span)
      }
      ExprKind::Ref(expr, _) => {
        let ty = self.resolve_expr_form(expr, Form::Place);
        self.types.new(TypeKind::Ref(ty))
      }
      ExprKind::Move(expr, _) => self.resolve_expr_form(expr, Form::Place),
      ExprKind::List(els) => {
        let item = self.types.new_var(span);
        for el in els {
          self.resolve_expr_form_type(el, Form::Value, item);
        }
        if let Some(list) = self.chart.builtins.list {
          self.types.new(TypeKind::Struct(list, vec![item]))
        } else {
          self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: "List" }))
        }
      }
      ExprKind::Method(receiver, name, generics, args) => {
        let (desugared, ty) = self.resolve_method(span, receiver, *name, generics, args);
        expr.kind = desugared;
        ty
      }
      ExprKind::Call(func, args) => {
        let func_ty = self.resolve_expr_form(func, Form::Value);
        self._resolve_call(span, func_ty, args)
      }
      ExprKind::Bool(_) => self.bool(span),
      ExprKind::Not(inner) => {
        let ty = self.resolve_expr_form(inner, Form::Value);
        let Some(op_fn) = self.chart.builtins.not else {
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.types.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._resolve_generics(
          &mut generics,
          self.chart.fn_generics(op_fn),
          true,
          Some(vec![ty, return_ty]),
        );
        if let ImplKind::Def(id, _) = generics.impls[0].kind {
          if self.chart.builtins.bool_not == Some(id) {
            return return_ty;
          }
        }
        expr.kind = ExprKind::Call(
          Box::new(Expr { span, kind: ExprKind::FnDef(op_fn, generics) }),
          vec![take(inner)],
        );
        return_ty
      }
      ExprKind::Neg(inner) => {
        let ty = self.resolve_expr_form(inner, Form::Value);
        let Some(op_fn) = self.chart.builtins.neg else {
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.types.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._resolve_generics(
          &mut generics,
          self.chart.fn_generics(op_fn),
          true,
          Some(vec![ty, return_ty]),
        );
        expr.kind = ExprKind::Call(
          Box::new(Expr { span, kind: ExprKind::FnDef(op_fn, generics) }),
          vec![take(inner)],
        );
        return_ty
      }
      ExprKind::BinaryOp(op, lhs, rhs) => {
        let lhs_ty = self.resolve_expr_form(lhs, Form::Value);
        let rhs_ty = self.resolve_expr_form(rhs, Form::Value);
        let Some(&Some(op_fn)) = self.chart.builtins.binary_ops.get(op) else {
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let return_ty = self.types.new_var(span);
        let mut generics = Generics { span, ..Default::default() };
        self._resolve_generics(
          &mut generics,
          self.chart.fn_generics(op_fn),
          true,
          Some(vec![lhs_ty, rhs_ty, return_ty]),
        );
        expr.kind = ExprKind::Call(
          Box::new(Expr { span, kind: ExprKind::FnDef(op_fn, generics) }),
          vec![take(lhs), take(rhs)],
        );
        return_ty
      }
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        let lhs_ty = self.resolve_expr_form(lhs, Form::Place);
        let rhs_ty = self.resolve_expr_form(rhs, Form::Value);
        let Some(&Some(op_fn)) = self.chart.builtins.binary_ops.get(op) else {
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let mut generics = Generics { span, ..Default::default() };
        self._resolve_generics(
          &mut generics,
          self.chart.fn_generics(op_fn),
          true,
          Some(vec![lhs_ty, rhs_ty, lhs_ty]),
        );
        expr.kind = ExprKind::CallAssign(
          Box::new(Expr { span, kind: ExprKind::FnDef(op_fn, generics) }),
          take(lhs),
          take(rhs),
        );
        self.types.nil()
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut ty = self.resolve_expr_pseudo_place(init);
        let mut err = false;
        for (_, expr) in cmps.iter_mut() {
          let other_ty = self.resolve_expr_pseudo_place(expr);
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
                self._resolve_generics(
                  &mut generics,
                  self.chart.fn_generics(op_fn),
                  true,
                  Some(vec![ty]),
                );
                Expr { span, kind: ExprKind::FnDef(op_fn, generics) }
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
        let cur_ty = self.resolve_expr_form(cur, Form::Value);
        let to_ty = self.resolve_type(to, true);
        let Some(cast_fn) = self.chart.builtins.cast else {
          return self.types.error(self.core.report(Diag::MissingOperatorBuiltin { span }));
        };
        let mut generics = Generics { span, ..Default::default() };
        self._resolve_generics(
          &mut generics,
          self.chart.fn_generics(cast_fn),
          true,
          Some(vec![cur_ty, to_ty]),
        );
        expr.kind = ExprKind::Call(
          Box::new(Expr { span, kind: ExprKind::FnDef(cast_fn, generics) }),
          vec![take(cur)],
        );
        to_ty
      }
      ExprKind::RangeExclusive(start, end) => {
        match self.resolve_range_expr(span, start, end, false) {
          Ok((expr_kind, ty)) => {
            expr.kind = expr_kind;
            ty
          }
          Err(e) => e,
        }
      }
      ExprKind::RangeInclusive(start, end) => {
        match self.resolve_range_expr(span, start, &mut Some(take(end)), true) {
          Ok((expr_kind, ty)) => {
            expr.kind = expr_kind;
            ty
          }
          Err(e) => e,
        }
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
            let ty = self.resolve_expr_form(expr, Form::Value);
            let mut generics = Generics { span, ..Default::default() };
            self._resolve_generics(
              &mut generics,
              self.chart.fn_generics(cast),
              true,
              Some(vec![ty, string_ty]),
            );
            expr.kind = ExprKind::Call(
              Box::new(Expr { span, kind: ExprKind::FnDef(cast, generics) }),
              vec![take(expr)],
            );
          }
        } else {
          for (expr, _) in rest {
            self.resolve_expr_form_type(expr, Form::Value, string_ty);
          }
        }
        string_ty
      }
      ExprKind::InlineIvy(binds, ty, _, _) => {
        for (_, value, expr) in binds {
          self.resolve_expr_form(expr, if *value { Form::Value } else { Form::Space });
        }
        self.resolve_type(ty, true)
      }
      ExprKind::Try(result) => {
        let Some(result_id) = self.chart.builtins.result else {
          return self
            .types
            .error(self.core.report(Diag::MissingBuiltin { span, builtin: "Result" }));
        };
        let ok = self.types.new_var(span);
        let err = self.types.new_var(span);
        let result_ty = self.types.new(TypeKind::Enum(result_id, vec![ok, err]));
        self.resolve_expr_form_type(result, Form::Value, result_ty);
        let return_ok = self.types.new_var(span);
        let return_result = self.types.new(TypeKind::Enum(result_id, vec![return_ok, err]));
        if let Some(return_ty) = &self.return_ty {
          if self.types.unify(*return_ty, return_result).is_failure() {
            self.core.report(Diag::TryBadReturnType {
              span,
              tried: self.types.show(self.chart, result_ty),
              ret: self.types.show(self.chart, *return_ty),
            });
          }
        } else {
          self.core.report(Diag::NoReturn { span });
        }
        ok
      }
      ExprKind::Is(..) | ExprKind::LogicalOp(..) => {
        self.enter_scope();
        self.resolve_cond(expr);
        self.exit_scope();
        self.bool(span)
      }
    }
  }

  fn resolve_cond(&mut self, cond: &mut Expr<'core>) {
    let span = cond.span;
    match &mut cond.kind {
      ExprKind::Error(_) => {}
      ExprKind::Is(expr, pat) => {
        let ty = self.resolve_expr_form(expr, Form::Value);
        self.resolve_pat_type(pat, Form::Value, true, ty);
      }
      ExprKind::LogicalOp(LogicalOp::And, a, b) => {
        self.resolve_cond(a);
        self.resolve_cond(b);
      }
      ExprKind::LogicalOp(LogicalOp::Or, a, b) => {
        self.enter_scope();
        self.resolve_cond(a);
        self.exit_scope();
        self.enter_scope();
        self.resolve_cond(b);
        self.exit_scope();
      }
      ExprKind::LogicalOp(LogicalOp::Implies, a, b) => {
        self.enter_scope();
        self.resolve_cond(a);
        self.resolve_cond(b);
        self.exit_scope();
      }
      _ => {
        let bool = self.bool(span);
        self.resolve_expr_form_type(cond, Form::Value, bool);
      }
    }
  }

  fn _resolve_call(&mut self, span: Span, func_ty: Type, args: &mut Vec<Expr<'core>>) -> Type {
    match self.fn_sig(span, func_ty, args.len()) {
      Ok((params, ret)) => {
        for (ty, arg) in params.into_iter().zip(args) {
          self.resolve_expr_form_type(arg, Form::Value, ty);
        }
        ret
      }
      Err(e) => {
        for arg in args {
          self.resolve_expr_form(arg, Form::Value);
        }
        self.types.error(self.core.report(e))
      }
    }
  }

  fn resolve_range_expr(
    &mut self,
    span: Span,
    start: &mut Option<Box<Expr<'core>>>,
    end: &mut Option<Box<Expr<'core>>>,
    end_inclusive: bool,
  ) -> Result<(ExprKind<'core>, Type), Type> {
    let bound_value_ty = self.types.new_var(span);
    let (left_bound, left_bound_ty) =
      self.resolve_range_bound(span, bound_value_ty, start, true)?;
    let (right_bound, right_bound_ty) =
      self.resolve_range_bound(span, bound_value_ty, end, end_inclusive)?;

    let Some(range_id) = self.chart.builtins.range else {
      return Err(
        self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: "Range" })),
      );
    };
    let generics = Generics { span, ..Default::default() };
    let expr_kind = ExprKind::Struct(
      range_id,
      generics,
      Box::new(Expr { span, kind: ExprKind::Tuple(vec![left_bound, right_bound]) }),
    );
    let ty = self.types.new(TypeKind::Struct(range_id, vec![left_bound_ty, right_bound_ty]));
    Ok((expr_kind, ty))
  }

  fn resolve_range_bound(
    &mut self,
    span: Span,
    bound_value_ty: Type,
    bound_value: &mut Option<Box<Expr<'core>>>,
    inclusive: bool,
  ) -> Result<(Expr<'core>, Type), Type> {
    let unbounded_id = self.chart.builtins.bound_unbounded.ok_or_else(|| {
      self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: "Unbounded" }))
    })?;
    let bound_id = if inclusive {
      self.chart.builtins.bound_inclusive.ok_or_else(|| {
        self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: "BoundInclusive" }))
      })?
    } else {
      self.chart.builtins.bound_exclusive.ok_or_else(|| {
        self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: "BoundExclusive" }))
      })?
    };
    let generics = Generics { span, ..Default::default() };
    match bound_value {
      Some(bound) => {
        self.resolve_expr_form_type(bound, Form::Value, bound_value_ty);
        Ok((
          Expr {
            span: bound.span,
            kind: ExprKind::Struct(bound_id, generics, Box::new(take(bound))),
          },
          self.types.new(TypeKind::Struct(bound_id, vec![bound_value_ty])),
        ))
      }
      None => Ok((
        Expr {
          span,
          kind: ExprKind::Struct(
            unbounded_id,
            generics,
            Box::new(Expr { span, kind: ExprKind::Tuple(vec![]) }),
          ),
        },
        self.types.new(TypeKind::Struct(unbounded_id, vec![])),
      )),
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

  fn resolve_expr_pseudo_place(&mut self, expr: &mut Expr<'core>) -> Type {
    let span = expr.span;
    let (form, ty) = self.resolve_expr(expr);
    if form == Form::Value {
      expr.wrap(|e| ExprKind::Place(e, Box::new(Expr { span, kind: ExprKind::Hole })));
    } else {
      self.coerce_expr(expr, form, Form::Place);
    }
    ty
  }

  fn builtin_ty(&mut self, span: Span, name: &'static str, builtin: Option<OpaqueTypeId>) -> Type {
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
