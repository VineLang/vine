use crate::{
  components::resolver::{Binding, Form, Resolver, Type},
  structures::{
    ast::{Expr, ExprKind, Ident, LogicalOp, Span},
    chart::{DefValueKind, FnId, OpaqueTypeId, StructId},
    diag::Diag,
    resolutions::FnRelId,
    tir::{TirExpr, TirExprKind, TirImpl},
    types::{Inverted, TypeKind},
  },
};

mod coerce_expr;
mod resolve_method;

impl<'core> Resolver<'core, '_> {
  pub(super) fn resolve_expr_form_type(
    &mut self,
    expr: &Expr<'core>,
    form: Form,
    ty: Type,
  ) -> TirExpr {
    let expr = self.resolve_expr_form(expr, form);
    self.expect_type(expr.span, expr.ty, ty);
    expr
  }

  pub(super) fn resolve_expr_form(&mut self, expr: &Expr<'core>, form: Form) -> TirExpr {
    let mut expr = self.resolve_expr(expr);
    self.coerce_expr(&mut expr, form);
    expr
  }

  pub(super) fn resolve_expr(&mut self, expr: &Expr<'core>) -> TirExpr {
    match self._resolve_expr(expr) {
      Ok((form, ty, kind)) => TirExpr { span: expr.span, ty, form, kind: Box::new(kind) },
      Err(diag) => self.error_expr(expr.span, diag),
    }
  }

  fn _resolve_expr(
    &mut self,
    expr: &Expr<'core>,
  ) -> Result<(Form, Type, TirExprKind), Diag<'core>> {
    let span = expr.span;
    Ok(match &expr.kind {
      ExprKind::Do(label, block) => {
        let ty = self.types.new_var(span);
        let (label, block) =
          self.bind_label(label, false, ty, |self_| self_.resolve_block_type(block, ty));
        (Form::Value, ty, TirExprKind::Do(label, block))
      }
      ExprKind::Assign(dir, space, value) => {
        let space = self.resolve_expr_form(space, Form::Space);
        let value = self.resolve_expr_form_type(value, Form::Value, space.ty);
        (Form::Value, self.types.nil(), TirExprKind::Assign(*dir, space, value))
      }
      ExprKind::Match(scrutinee, arms) => {
        let scrutinee = self.resolve_expr_form(scrutinee, Form::Value);
        let result = self.types.new_var(span);
        let arms = Vec::from_iter(arms.iter().map(|(pat, block)| {
          self.enter_scope();
          let pat = self.resolve_pat_type(pat, Form::Value, true, scrutinee.ty);
          let block = self.resolve_block_type(block, result);
          self.exit_scope();
          (pat, block)
        }));
        (Form::Value, result, TirExprKind::Match(scrutinee, arms))
      }
      ExprKind::If(arms, leg) => {
        let result = if leg.is_some() { self.types.new_var(span) } else { self.types.nil() };
        let arms = Vec::from_iter(arms.iter().map(|(cond, block)| {
          self.enter_scope();
          let cond = self.resolve_cond(cond);
          let block = self.resolve_block_type(block, result);
          self.exit_scope();
          (cond, block)
        }));
        let leg = leg.as_ref().map(|leg| self.resolve_block_type(leg, result));
        (Form::Value, result, TirExprKind::If(arms, leg))
      }
      ExprKind::While(label, cond, block) => {
        let nil = self.types.nil();
        let (label, (cond, block)) = self.bind_label(label, true, nil, |self_| {
          self_.enter_scope();
          let cond = self_.resolve_cond(cond);
          let block = self_.resolve_block(block);
          self_.exit_scope();
          (cond, block)
        });
        (Form::Value, nil, TirExprKind::While(label, cond, block))
      }
      ExprKind::Loop(label, block) => {
        let result = self.types.new_var(span);
        let (label, block) =
          self.bind_label(label, true, result, |self_| self_.resolve_block(block));
        (Form::Value, result, TirExprKind::Loop(label, block))
      }
      ExprKind::Fn(params, ret, body) => {
        let (ty, closure_id) = self.resolve_closure(params, ret, body, true);
        (Form::Value, ty, TirExprKind::Closure(closure_id))
      }
      ExprKind::Break(label, value) => {
        let nil = self.types.nil();
        let label_info = if let Some(label) = *label {
          self.labels.get(&label).copied().ok_or(Diag::UnboundLabel { span, label })
        } else {
          self.loops.last().copied().ok_or(Diag::NoLoopBreak { span })
        };
        match label_info {
          Ok(label_info) => {
            let value = match value {
              Some(value) => {
                Some(self.resolve_expr_form_type(value, Form::Value, label_info.break_ty))
              }
              None => {
                if self.types.unify(label_info.break_ty, nil).is_failure() {
                  self.core.report(Diag::MissingBreakExpr {
                    span,
                    ty: self.types.show(self.chart, label_info.break_ty),
                  });
                }
                None
              }
            };
            (Form::Value, self.types.new_var(span), TirExprKind::Break(label_info.id, value))
          }
          Err(diag) => {
            if let Some(value) = value {
              self.resolve_expr_form(value, Form::Value);
            }
            Err(diag)?
          }
        }
      }
      ExprKind::Return(value) => {
        let nil = self.types.nil();
        if let Some(ty) = &self.return_ty {
          let ty = *ty;
          let value = match value {
            Some(value) => Some(self.resolve_expr_form_type(value, Form::Value, ty)),
            None => {
              if self.types.unify(ty, nil).is_failure() {
                self
                  .core
                  .report(Diag::MissingReturnExpr { span, ty: self.types.show(self.chart, ty) });
              }
              None
            }
          };
          (Form::Value, self.types.new_var(span), TirExprKind::Return(value))
        } else {
          Err(Diag::NoReturn { span })?
        }
      }
      ExprKind::Continue(label) => {
        let label_info = if let Some(label) = *label {
          let label_info = *self.labels.get(&label).ok_or(Diag::UnboundLabel { span, label })?;
          if !label_info.is_loop {
            Err(Diag::NoContinueLabel { span, label })?
          }
          label_info
        } else {
          self.loops.last().copied().ok_or(Diag::NoLoopContinue { span })?
        };
        (Form::Value, self.types.new_var(span), TirExprKind::Continue(label_info.id))
      }
      ExprKind::Ref(inner, _) => {
        let inner = self.resolve_expr_form(inner, Form::Place);
        (Form::Value, self.types.new(TypeKind::Ref(inner.ty)), TirExprKind::Ref(inner))
      }
      ExprKind::Move(inner, _) => {
        let inner = self.resolve_expr_form(inner, Form::Place);
        (Form::Value, inner.ty, TirExprKind::Move(inner))
      }
      ExprKind::List(elements) => {
        let ty = self.types.new_var(span);
        let elements = Vec::from_iter(
          elements.iter().map(|element| self.resolve_expr_form_type(element, Form::Value, ty)),
        );
        let Some(list) = self.chart.builtins.list else {
          Err(Diag::MissingBuiltin { span, builtin: "List" })?
        };
        (Form::Value, self.types.new(TypeKind::Struct(list, vec![ty])), TirExprKind::List(elements))
      }
      ExprKind::Method(receiver, name, generics, args) => {
        self.resolve_method(span, receiver, *name, generics, args)?
      }
      ExprKind::Call(func, args) => {
        let func = self.resolve_expr(func);
        self._resolve_call(span, func, args)?
      }
      ExprKind::Bool(bool) => (Form::Value, self.bool(span), TirExprKind::Bool(*bool)),
      ExprKind::Not(inner) => {
        let inner = self.resolve_expr_form(inner, Form::Value);
        let return_ty = self.types.new_var(span);
        let rel = self.builtin_fn(span, self.chart.builtins.not, "not", [inner.ty, return_ty])?;
        if let Some(TirImpl::Def(id, _)) = self.fn_rels[rel].1.first() {
          if self.chart.builtins.bool_not == Some(*id) {
            return Ok((Form::Value, return_ty, TirExprKind::Not(inner)));
          }
        }
        (Form::Value, return_ty, TirExprKind::CallFn(rel, vec![inner]))
      }
      ExprKind::Neg(inner) => {
        let inner = self.resolve_expr_form(inner, Form::Value);
        let return_ty = self.types.new_var(span);
        let rel = self.builtin_fn(span, self.chart.builtins.neg, "neg", [inner.ty, return_ty])?;
        (Form::Value, return_ty, TirExprKind::CallFn(rel, vec![inner]))
      }
      ExprKind::BinaryOp(op, lhs, rhs) => {
        let lhs = self.resolve_expr_form(lhs, Form::Value);
        let rhs = self.resolve_expr_form(rhs, Form::Value);
        let return_ty = self.types.new_var(span);
        let fn_id = self.chart.builtins.binary_ops.get(op).copied().flatten();
        let rel = self.builtin_fn(span, fn_id, op.as_str(), [lhs.ty, rhs.ty, return_ty])?;
        (Form::Value, return_ty, TirExprKind::CallFn(rel, vec![lhs, rhs]))
      }
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        let lhs = self.resolve_expr_form(lhs, Form::Place);
        let rhs = self.resolve_expr_form(rhs, Form::Value);
        let fn_id = self.chart.builtins.binary_ops.get(op).copied().flatten();
        let rel = self.builtin_fn(span, fn_id, op.as_str(), [lhs.ty, rhs.ty, lhs.ty])?;
        (Form::Value, self.types.nil(), TirExprKind::CallAssign(rel, lhs, rhs))
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let init = self.resolve_expr_pseudo_place(init);
        let mut err = Ok(());
        let mut ty = init.ty;
        let mut rhs = Vec::new();
        for (_, expr) in cmps.iter() {
          let other = self.resolve_expr_pseudo_place(expr);
          if self.types.unify(ty, other.ty).is_failure() {
            err = Err(self.core.report(Diag::CannotCompare {
              span,
              lhs: self.types.show(self.chart, ty),
              rhs: self.types.show(self.chart, other.ty),
            }));
            ty = other.ty;
          }
          rhs.push(other);
        }
        err?;
        let cmps = cmps
          .iter()
          .zip(rhs)
          .map(|((op, _), rhs)| {
            let fn_id = self.chart.builtins.comparison_ops.get(op).copied().flatten();
            let rel = self.builtin_fn(span, fn_id, op.as_str(), [ty])?;
            Ok((rel, rhs))
          })
          .collect::<Result<Vec<_>, Diag>>()?;
        (Form::Value, self.bool(span), TirExprKind::CallCompare(init, cmps))
      }
      ExprKind::Cast(inner, to, _) => {
        let inner = self.resolve_expr_form(inner, Form::Value);
        let to_ty = self.resolve_type(to, true);
        let rel = self.builtin_fn(span, self.chart.builtins.cast, "cast", [inner.ty, to_ty])?;
        (Form::Value, to_ty, TirExprKind::CallFn(rel, vec![inner]))
      }
      ExprKind::RangeExclusive(start, end) => {
        self.resolve_range_expr(span, start.as_deref(), end.as_deref(), false)?
      }
      ExprKind::RangeInclusive(start, end) => {
        self.resolve_range_expr(span, start.as_deref(), Some(end), true)?
      }
      ExprKind::N32(n) => {
        (Form::Value, self.builtin_ty(span, "N32", self.chart.builtins.n32), TirExprKind::N32(*n))
      }
      ExprKind::I32(n) => {
        (Form::Value, self.builtin_ty(span, "I32", self.chart.builtins.i32), TirExprKind::I32(*n))
      }
      ExprKind::F32(n) => {
        (Form::Value, self.builtin_ty(span, "F32", self.chart.builtins.f32), TirExprKind::F32(*n))
      }
      ExprKind::Char(c) => (
        Form::Value,
        self.builtin_ty(span, "Char", self.chart.builtins.char),
        TirExprKind::Char(*c),
      ),
      ExprKind::String(init, rest) => {
        let string_ty = if let Some(string) = self.chart.builtins.string {
          self.types.new(TypeKind::Struct(string, Vec::new()))
        } else {
          self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: "String" }))
        };
        let rest = rest
          .iter()
          .map(|(expr, str)| {
            let span = expr.span;
            let expr = self.resolve_expr_form(expr, Form::Value);
            let rel = self.builtin_fn(span, self.chart.builtins.cast, "cast", [expr.ty, string_ty]);
            let expr = if let Ok(rel) = rel {
              TirExpr {
                span,
                ty: string_ty,
                form: Form::Value,
                kind: Box::new(TirExprKind::CallFn(rel, vec![expr])),
              }
            } else {
              self.expect_type(span, expr.ty, string_ty);
              expr
            };
            (expr, str.content.clone())
          })
          .collect();
        (Form::Value, string_ty, TirExprKind::String(init.content.clone(), rest))
      }
      ExprKind::InlineIvy(binds, ty, _, net) => {
        let binds = Vec::from_iter(binds.iter().map(|(name, value, expr)| {
          (
            name.0 .0.into(),
            *value,
            self.resolve_expr_form(expr, if *value { Form::Value } else { Form::Space }),
          )
        }));
        let ty = self.resolve_type(ty, true);
        (Form::Value, ty, TirExprKind::InlineIvy(binds, net.clone()))
      }
      ExprKind::Try(result) => {
        let Some(result_id) = self.chart.builtins.result else {
          Err(Diag::MissingBuiltin { span, builtin: "Result" })?
        };
        let ok = self.types.new_var(span);
        let err = self.types.new_var(span);
        let result_ty = self.types.new(TypeKind::Enum(result_id, vec![ok, err]));
        let result = self.resolve_expr_form_type(result, Form::Value, result_ty);
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
          Err(Diag::NoReturn { span })?
        }
        (Form::Value, ok, TirExprKind::Try(result))
      }
      ExprKind::Is(..) | ExprKind::LogicalOp(..) => {
        self.enter_scope();
        let kind = self._resolve_cond(expr)?;
        self.exit_scope();
        (Form::Value, self.bool(span), kind)
      }
      ExprKind::Paren(e) => self._resolve_expr(e)?,
      ExprKind::Error(e) => Err(*e)?,
      ExprKind::Hole => (Form::Space, self.types.new_var(span), TirExprKind::Hole),
      ExprKind::Deref(inner, _) => {
        let ty = self.types.new_var(span);
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        let inner = self.resolve_expr_form_type(inner, Form::Value, ref_ty);
        (Form::Place, ty, TirExprKind::Deref(inner))
      }
      ExprKind::Inverse(inner, _) => {
        let inner = self.resolve_expr(inner);
        (inner.form.inverse(), inner.ty.inverse(), TirExprKind::Inverse(inner))
      }
      ExprKind::Place(value, space) => {
        let value = self.resolve_expr_form(value, Form::Value);
        let space = self.resolve_expr_form(space, Form::Space);
        let ty = if self.types.unify(value.ty, space.ty).is_failure() {
          self.types.error(self.core.report(Diag::MismatchedValueSpaceTypes {
            span,
            value: self.types.show(self.chart, value.ty),
            space: self.types.show(self.chart, space.ty),
          }))
        } else {
          value.ty
        };
        (Form::Place, ty, TirExprKind::Place(value, space))
      }
      ExprKind::TupleField(tuple, index, _) => {
        let tuple = self.resolve_expr(tuple);
        if tuple.form == Form::Space {
          self.core.report(Diag::SpaceField { span });
        }
        let (ty, len) = self.tuple_field(span, tuple.ty, *index).ok_or_else(|| {
          Diag::MissingTupleField { span, ty: self.types.show(self.chart, tuple.ty), i: *index }
        })?;
        (tuple.form, ty, TirExprKind::Field(tuple, *index, len))
      }
      ExprKind::ObjectField(object, key) => {
        let object = self.resolve_expr(object);
        if object.form == Form::Space {
          self.core.report(Diag::SpaceField { span });
        }
        let (ty, index, len) = self.object_field(span, object.ty, key.ident).ok_or_else(|| {
          Diag::MissingObjectField {
            span: key.span,
            ty: self.types.show(self.chart, object.ty),
            key: key.ident,
          }
        })?;
        (object.form, ty, TirExprKind::Field(object, index, len))
      }
      ExprKind::Tuple(elements) => self._resolve_tuple(span, elements),
      ExprKind::Object(entries) => {
        let mut object = self.build_object(entries, Self::resolve_expr)?;
        let form = self.composite_form(span, object.values().map(|x| x.form));
        for element in object.values_mut() {
          self.coerce_expr(element, form);
        }
        let ty = self.types.new(TypeKind::Object(object.iter().map(|(&i, x)| (i, x.ty)).collect()));
        (form, ty, TirExprKind::Composite(object.into_values().collect()))
      }
      ExprKind::Unwrap(inner) => {
        let inner = self.resolve_expr(inner);

        let data_ty = match self.types.force_kind(self.core, inner.ty) {
          (inv, TypeKind::Struct(struct_id, type_params)) => {
            let struct_id = *struct_id;
            let type_params = &type_params.clone();
            self.get_struct_data(span, inner.ty, struct_id, type_params).invert_if(inv)
          }
          _ => self.types.error(self.core.report(Diag::UnwrapNonStruct { span })),
        };

        (inner.form, data_ty, TirExprKind::Unwrap(inner))
      }
      ExprKind::Path(path, args) => {
        let mut args = args.as_ref();
        let (form, ty, kind) = 'inner: {
          if let Some(ident) = path.as_ident() {
            if let Some(bind) = self.scope.get(&ident).and_then(|x| x.last()) {
              break 'inner match bind.binding {
                Binding::Local(local, ty) => (Form::Place, ty, TirExprKind::Local(local)),
                Binding::Closure(id, ty) => (Form::Value, ty, TirExprKind::Closure(id)),
              };
            }
          }
          match self.resolve_path_to(self.cur_def, path, "value", |d| d.value_kind) {
            Ok(DefValueKind::Struct(struct_id)) => {
              let (type_params, _) =
                self.resolve_generics(path, self.chart.structs[struct_id].generics, true);
              let data_ty =
                self.types.import(&self.sigs.structs[struct_id], Some(&type_params)).data;
              let data = if let Some(args) = args.take() {
                if let [data] = &**args {
                  self.resolve_expr(data)
                } else {
                  let (form, ty, kind) = self._resolve_tuple(span, args);
                  TirExpr { span, form, ty, kind: Box::new(kind) }
                }
              } else {
                self.error_expr(span, Diag::ExpectedDataExpr { span })
              };
              if self.types.unify(data.ty, data_ty).is_failure() {
                self.core.report(Diag::ExpectedTypeFound {
                  span: expr.span,
                  expected: self.types.show(self.chart, data_ty),
                  found: self.types.show(self.chart, data.ty),
                });
              }
              let ty = self.types.new(TypeKind::Struct(struct_id, type_params));
              (data.form, ty, TirExprKind::Struct(struct_id, data))
            }
            Ok(DefValueKind::Enum(enum_id, variant_id)) => {
              let (type_params, _) =
                self.resolve_generics(path, self.chart.enums[enum_id].generics, true);
              let data_ty =
                self.types.import_with(&self.sigs.enums[enum_id], Some(&type_params), |t, sig| {
                  Some(t.transfer(&sig.variant_data[variant_id]?))
                });
              let data = args.take().map(|args| {
                if let [data] = &**args {
                  self.resolve_expr(data)
                } else {
                  let (form, ty, kind) = self._resolve_tuple(span, args);
                  TirExpr { span, form, ty, kind: Box::new(kind) }
                }
              });
              let data = match data_ty {
                Some(data_ty) => Some(match data {
                  Some(mut data) => {
                    self.coerce_expr(&mut data, Form::Value);
                    self.expect_type(data.span, data.ty, data_ty);
                    data
                  }
                  None => self.error_expr(span, Diag::ExpectedDataExpr { span }),
                }),
                None => {
                  if data.is_some() {
                    self.core.report(Diag::EnumVariantNoData { span });
                  }
                  None
                }
              };
              let ty = self.types.new(TypeKind::Enum(enum_id, type_params));
              (Form::Value, ty, TirExprKind::Enum(enum_id, variant_id, data))
            }
            Ok(DefValueKind::Const(const_id)) => {
              let (type_params, impl_params) =
                self.resolve_generics(path, self.chart.const_generics(const_id), true);
              let ty = self.types.import(self.sigs.const_sig(const_id), Some(&type_params)).ty;
              let rel = self.const_rels.push((const_id, impl_params));
              (Form::Value, ty, TirExprKind::Const(rel))
            }
            Ok(DefValueKind::Fn(fn_id)) => {
              let (type_params, impl_params) =
                self.resolve_generics(path, self.chart.fn_generics(fn_id), true);
              let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
              let ty = self.types.new(TypeKind::Fn(sig.params, sig.ret_ty));
              let rel = self.fn_rels.push((fn_id, impl_params));
              (Form::Value, ty, TirExprKind::Fn(rel))
            }
            Err(diag) => Err(diag)?,
          }
        };
        if let Some(args) = args {
          self._resolve_call(span, TirExpr { span, ty, form, kind: Box::new(kind) }, args)?
        } else {
          (form, ty, kind)
        }
      }
    })
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
      (_, TypeKind::Error(_)) => Some((ty, index + 1)),
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
      (_, TypeKind::Error(_)) => Some((ty, 0, 1)),
      _ => None,
    }
  }

  fn composite_form(&mut self, span: Span, forms: impl IntoIterator<Item = Form> + Clone) -> Form {
    let space = forms.clone().into_iter().any(|x| x == Form::Space);
    let value = forms.clone().into_iter().any(|x| x == Form::Value);
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

  fn _resolve_tuple(
    &mut self,
    span: Span,
    elements: &Vec<Expr<'core>>,
  ) -> (Form, Type, TirExprKind) {
    let mut elements = elements.iter().map(|x| self.resolve_expr(x)).collect::<Vec<_>>();
    let form = self.composite_form(span, elements.iter().map(|x| x.form));
    for element in &mut elements {
      self.coerce_expr(element, form);
    }
    let ty = self.types.new(TypeKind::Tuple(elements.iter().map(|x| x.ty).collect()));
    (form, ty, TirExprKind::Composite(elements))
  }

  fn resolve_cond(&mut self, cond: &Expr<'core>) -> TirExpr {
    match self._resolve_cond(cond) {
      Ok(kind) => {
        let ty = self.bool(cond.span);
        TirExpr { span: cond.span, ty, form: Form::Value, kind: Box::new(kind) }
      }
      Err(diag) => self.error_expr(cond.span, diag),
    }
  }

  fn _resolve_cond(&mut self, cond: &Expr<'core>) -> Result<TirExprKind, Diag<'core>> {
    let span = cond.span;
    Ok(match &cond.kind {
      ExprKind::Error(err) => Err(*err)?,
      ExprKind::Is(expr, pat) => {
        let expr = self.resolve_expr_form(expr, Form::Value);
        let pat = self.resolve_pat_type(pat, Form::Value, true, expr.ty);
        TirExprKind::Is(expr, pat)
      }
      ExprKind::LogicalOp(LogicalOp::And, a, b) => {
        let a = self.resolve_cond(a);
        let b = self.resolve_cond(b);
        TirExprKind::LogicalOp(LogicalOp::And, a, b)
      }
      ExprKind::LogicalOp(LogicalOp::Or, a, b) => {
        self.enter_scope();
        let a = self.resolve_cond(a);
        self.exit_scope();
        self.enter_scope();
        let b = self.resolve_cond(b);
        self.exit_scope();
        TirExprKind::LogicalOp(LogicalOp::Or, a, b)
      }
      ExprKind::LogicalOp(LogicalOp::Implies, a, b) => {
        self.enter_scope();
        let a = self.resolve_cond(a);
        let b = self.resolve_cond(b);
        self.exit_scope();
        TirExprKind::LogicalOp(LogicalOp::Implies, a, b)
      }
      _ => {
        let bool = self.bool(span);
        *self.resolve_expr_form_type(cond, Form::Value, bool).kind
      }
    })
  }

  fn _resolve_call(
    &mut self,
    span: Span,
    mut func: TirExpr,
    args: &Vec<Expr<'core>>,
  ) -> Result<(Form, Type, TirExprKind), Diag<'core>> {
    self.coerce_expr(&mut func, Form::Value);
    match self.fn_sig(span, func.ty, args.len()) {
      Ok((params, ret)) => {
        let args = args
          .iter()
          .zip(params)
          .map(|(arg, ty)| self.resolve_expr_form_type(arg, Form::Value, ty))
          .collect();
        Ok((Form::Value, ret, TirExprKind::Call(func, args)))
      }
      Err(diag) => {
        for arg in args {
          self.resolve_expr_form(arg, Form::Value);
        }
        Err(diag)
      }
    }
  }

  fn resolve_range_expr(
    &mut self,
    span: Span,
    start: Option<&Expr<'core>>,
    end: Option<&Expr<'core>>,
    end_inclusive: bool,
  ) -> Result<(Form, Type, TirExprKind), Diag<'core>> {
    let bound_value_ty = self.types.new_var(span);
    let left_bound = self.resolve_range_bound(span, bound_value_ty, start, true)?;
    let right_bound = self.resolve_range_bound(span, bound_value_ty, end, end_inclusive)?;
    let Some(range_id) = self.chart.builtins.range else {
      Err(Diag::MissingBuiltin { span, builtin: "Range" })?
    };
    let ty = self.types.new(TypeKind::Struct(range_id, vec![left_bound.ty, right_bound.ty]));
    Ok((
      Form::Value,
      ty,
      TirExprKind::Struct(
        range_id,
        TirExpr {
          span,
          ty: self.types.new(TypeKind::Tuple(vec![left_bound.ty, right_bound.ty])),
          form: Form::Value,
          kind: Box::new(TirExprKind::Composite(vec![left_bound, right_bound])),
        },
      ),
    ))
  }

  fn resolve_range_bound(
    &mut self,
    span: Span,
    bound_value_ty: Type,
    bound_value: Option<&Expr<'core>>,
    inclusive: bool,
  ) -> Result<TirExpr, Diag<'core>> {
    match bound_value {
      Some(bound) => {
        let bound = self.resolve_expr_form_type(bound, Form::Value, bound_value_ty);
        let (bound_id, bound_name) = if inclusive {
          (self.chart.builtins.bound_inclusive, "BoundInclusive")
        } else {
          (self.chart.builtins.bound_exclusive, "BoundExclusive")
        };
        let Some(bound_id) = bound_id else {
          Err(Diag::MissingBuiltin { span, builtin: bound_name })?
        };
        Ok(TirExpr {
          span: bound.span,
          ty: self.types.new(TypeKind::Struct(bound_id, vec![bound_value_ty])),
          form: Form::Value,
          kind: Box::new(TirExprKind::Struct(bound_id, bound)),
        })
      }
      None => {
        let Some(unbounded_id) = self.chart.builtins.bound_unbounded else {
          Err(Diag::MissingBuiltin { span, builtin: "Unbounded" })?
        };
        Ok(TirExpr {
          span,
          ty: self.types.new(TypeKind::Struct(unbounded_id, vec![])),
          form: Form::Value,
          kind: Box::new(TirExprKind::Struct(
            unbounded_id,
            TirExpr {
              span,
              ty: self.types.nil(),
              form: Form::Value,
              kind: Box::new(TirExprKind::Composite(vec![])),
            },
          )),
        })
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

  fn resolve_expr_pseudo_place(&mut self, expr: &Expr<'core>) -> TirExpr {
    let mut expr = self.resolve_expr(expr);
    let span = expr.span;
    let ty = expr.ty;
    if expr.form == Form::Value {
      TirExpr {
        span,
        ty: expr.ty,
        form: Form::Place,
        kind: Box::new(TirExprKind::Place(
          expr,
          TirExpr { span, ty, form: Form::Space, kind: Box::new(TirExprKind::Hole) },
        )),
      }
    } else {
      self.coerce_expr(&mut expr, Form::Place);
      expr
    }
  }

  pub(super) fn builtin_ty(
    &mut self,
    span: Span,
    name: &'static str,
    builtin: Option<OpaqueTypeId>,
  ) -> Type {
    if let Some(id) = builtin {
      self.types.new(TypeKind::Opaque(id, vec![]))
    } else {
      self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: name }))
    }
  }

  fn bool(&mut self, span: Span) -> Type {
    self.builtin_ty(span, "Bool", self.chart.builtins.bool)
  }

  fn builtin_fn<const N: usize>(
    &mut self,
    span: Span,
    fn_id: Option<FnId>,
    name: &'static str,
    type_params: [Type; N],
  ) -> Result<FnRelId, Diag<'core>> {
    if let Some(fn_id) = fn_id {
      let (_, impl_params) = self._resolve_generics(
        span,
        None,
        self.chart.fn_generics(fn_id),
        true,
        Some(Vec::from(type_params)),
      );
      Ok(self.fn_rels.push((fn_id, impl_params)))
    } else {
      Err(Diag::MissingBuiltin { span, builtin: name })?
    }
  }
}
