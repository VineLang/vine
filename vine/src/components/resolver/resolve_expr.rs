use crate::{
  components::resolver::{Binding, Resolver, Type},
  structures::{
    ast::{Expr, ExprKind, Ident, LogicalOp, Span},
    chart::{DefValueKind, FnId, GenericsId, OpaqueTypeId, StructId},
    diag::Diag,
    resolutions::{FnRel, FnRelId},
    tir::{TirExpr, TirExprKind, TirImpl},
    types::{ImplType, TypeKind},
  },
};

mod resolve_method;

impl<'core> Resolver<'core, '_> {
  pub(super) fn resolve_expr_type(&mut self, expr: &Expr<'core>, ty: Type) -> TirExpr {
    let expr = self.resolve_expr(expr);
    self.expect_type(expr.span, expr.ty, ty);
    expr
  }

  pub(super) fn resolve_expr(&mut self, expr: &Expr<'core>) -> TirExpr {
    match self._resolve_expr(expr) {
      Ok((ty, kind)) => TirExpr { span: expr.span, ty, kind: Box::new(kind) },
      Err(diag) => self.error_expr(expr.span, diag),
    }
  }

  fn _resolve_expr(&mut self, expr: &Expr<'core>) -> Result<(Type, TirExprKind), Diag<'core>> {
    let span = expr.span;
    Ok(match &expr.kind {
      ExprKind::Do(label, block) => {
        let ty = self.types.new_var(span);
        let (label, block) =
          self.bind_label(label, false, ty, |self_| self_.resolve_block_type(block, ty));
        (ty, TirExprKind::Do(label, block))
      }
      ExprKind::Assign(dir, space, value) => {
        let space = self.resolve_expr(space);
        let value = self.resolve_expr_type(value, space.ty);
        (self.types.nil(), TirExprKind::Assign(*dir, space, value))
      }
      ExprKind::Match(scrutinee, arms) => {
        let scrutinee = self.resolve_expr(scrutinee);
        let result = self.types.new_var(span);
        let arms = Vec::from_iter(arms.iter().map(|(pat, block)| {
          self.enter_scope();
          let pat = self.resolve_pat_type(pat, scrutinee.ty);
          let block = self.resolve_block_type(block, result);
          self.exit_scope();
          (pat, block)
        }));
        (result, TirExprKind::Match(scrutinee, arms))
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
        (result, TirExprKind::If(arms, leg))
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
        (nil, TirExprKind::While(label, cond, block))
      }
      ExprKind::For(label, pat, iter, block) => {
        let nil = self.types.nil();
        let (label, result) = self.bind_label(label, true, nil, |self_| {
          self_.enter_scope();
          let iter = self_.resolve_expr(iter);
          let pat = self_.resolve_pat(pat);
          let rel =
            self_.builtin_fn(span, self_.chart.builtins.advance, "advance", [iter.ty, pat.ty])?;
          let block = self_.resolve_block(block);
          self_.exit_scope();
          Result::<_, Diag<'core>>::Ok((rel, pat, iter, block))
        });
        let (rel, pat, iter, block) = result?;
        (nil, TirExprKind::For(label, rel, pat, iter, block))
      }
      ExprKind::Loop(label, block) => {
        let result = self.types.new_var(span);
        let (label, block) =
          self.bind_label(label, true, result, |self_| self_.resolve_block(block));
        (result, TirExprKind::Loop(label, block))
      }
      ExprKind::Fn(flex, params, ret, body) => {
        let (ty, closure_id) = self.resolve_closure(span, *flex, params, ret, body, true);
        (ty, TirExprKind::Closure(closure_id))
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
              Some(value) => Some(self.resolve_expr_type(value, label_info.break_ty)),
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
            (self.types.new_var(span), TirExprKind::Break(label_info.id, value))
          }
          Err(diag) => {
            if let Some(value) = value {
              self.resolve_expr(value);
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
            Some(value) => Some(self.resolve_expr_type(value, ty)),
            None => {
              if self.types.unify(ty, nil).is_failure() {
                self
                  .core
                  .report(Diag::MissingReturnExpr { span, ty: self.types.show(self.chart, ty) });
              }
              None
            }
          };
          (self.types.new_var(span), TirExprKind::Return(value))
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
        (self.types.new_var(span), TirExprKind::Continue(label_info.id))
      }
      ExprKind::Ref(inner, _) => {
        let inner = self.resolve_expr(inner);
        (self.types.new(TypeKind::Ref(inner.ty)), TirExprKind::Ref(inner))
      }
      ExprKind::List(elements) => {
        let ty = self.types.new_var(span);
        let elements =
          Vec::from_iter(elements.iter().map(|element| self.resolve_expr_type(element, ty)));
        let Some(list) = self.chart.builtins.list else {
          Err(Diag::MissingBuiltin { span, builtin: "List" })?
        };
        (self.types.new(TypeKind::Struct(list, vec![ty])), TirExprKind::List(elements))
      }
      ExprKind::Method(receiver, name, generics, args) => {
        self.resolve_method(span, receiver, *name, generics, args)?
      }
      ExprKind::Call(func, args) => {
        let func = self.resolve_expr(func);
        self._resolve_call(span, func, args)?
      }
      ExprKind::Bool(bool) => (self.bool(span), TirExprKind::Bool(*bool)),
      ExprKind::Not(inner) => {
        let inner = self.resolve_expr(inner);
        let return_ty = self.types.new_var(span);
        let rel = self.builtin_fn(span, self.chart.builtins.not, "not", [inner.ty, return_ty])?;
        if let FnRel::Item(FnId::Abstract(..), impls) = &self.rels.fns[rel] {
          if let [TirImpl::Def(id, _)] = **impls {
            if self.chart.builtins.bool_not == Some(id) {
              return Ok((return_ty, TirExprKind::Not(inner)));
            }
          }
        }
        (return_ty, TirExprKind::Call(rel, None, vec![inner]))
      }
      ExprKind::Neg(inner) => {
        let inner = self.resolve_expr(inner);
        let return_ty = self.types.new_var(span);
        let rel = self.builtin_fn(span, self.chart.builtins.neg, "neg", [inner.ty, return_ty])?;
        (return_ty, TirExprKind::Call(rel, None, vec![inner]))
      }
      ExprKind::BinaryOp(op, lhs, rhs) => {
        let lhs = self.resolve_expr(lhs);
        let rhs = self.resolve_expr(rhs);
        let return_ty = self.types.new_var(span);
        let fn_id = self.chart.builtins.binary_ops.get(op).copied().flatten();
        let rel = self.builtin_fn(span, fn_id, op.as_str(), [lhs.ty, rhs.ty, return_ty])?;
        (return_ty, TirExprKind::Call(rel, None, vec![lhs, rhs]))
      }
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        let lhs = self.resolve_expr(lhs);
        let rhs = self.resolve_expr(rhs);
        let fn_id = self.chart.builtins.binary_ops.get(op).copied().flatten();
        let rel = self.builtin_fn(span, fn_id, op.as_str(), [lhs.ty, rhs.ty, lhs.ty])?;
        (self.types.nil(), TirExprKind::CallAssign(rel, lhs, rhs))
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let init = self.resolve_expr(init);
        let mut err = Ok(());
        let mut ty = init.ty;
        let mut rhs = Vec::new();
        for (_, expr) in cmps.iter() {
          let other = self.resolve_expr(expr);
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
        (self.bool(span), TirExprKind::CallCompare(init, cmps))
      }
      ExprKind::Cast(inner, to, _) => {
        let inner = self.resolve_expr(inner);
        let to_ty = self.resolve_type(to, true);
        let rel = self.builtin_fn(span, self.chart.builtins.cast, "cast", [inner.ty, to_ty])?;
        (to_ty, TirExprKind::Call(rel, None, vec![inner]))
      }
      ExprKind::RangeExclusive(start, end) => {
        self.resolve_range_expr(span, start.as_deref(), end.as_deref(), false)?
      }
      ExprKind::RangeInclusive(start, end) => {
        self.resolve_range_expr(span, start.as_deref(), Some(end), true)?
      }
      ExprKind::N32(n) => {
        (self.builtin_ty(span, "N32", self.chart.builtins.n32), TirExprKind::N32(*n))
      }
      ExprKind::I32(n) => {
        (self.builtin_ty(span, "I32", self.chart.builtins.i32), TirExprKind::I32(*n))
      }
      ExprKind::F32(n) => {
        (self.builtin_ty(span, "F32", self.chart.builtins.f32), TirExprKind::F32(*n))
      }
      ExprKind::Char(c) => {
        (self.builtin_ty(span, "Char", self.chart.builtins.char), TirExprKind::Char(*c))
      }
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
            let expr = self.resolve_expr(expr);
            let rel = self.builtin_fn(span, self.chart.builtins.cast, "cast", [expr.ty, string_ty]);
            let expr = if let Ok(rel) = rel {
              TirExpr {
                span,
                ty: string_ty,
                kind: Box::new(TirExprKind::Call(rel, None, vec![expr])),
              }
            } else {
              self.expect_type(span, expr.ty, string_ty);
              expr
            };
            (expr, str.content.clone())
          })
          .collect();
        (string_ty, TirExprKind::String(init.content.clone(), rest))
      }
      ExprKind::InlineIvy(binds, ty, _, net) => {
        let binds = Vec::from_iter(
          binds
            .iter()
            .map(|(name, value, expr)| (name.0 .0.into(), *value, self.resolve_expr(expr))),
        );
        let ty = self.resolve_type(ty, true);
        (ty, TirExprKind::InlineIvy(binds, net.clone()))
      }
      ExprKind::Try(result) => {
        let Some(result_id) = self.chart.builtins.result else {
          Err(Diag::MissingBuiltin { span, builtin: "Result" })?
        };
        let ok = self.types.new_var(span);
        let err = self.types.new_var(span);
        let result_ty = self.types.new(TypeKind::Enum(result_id, vec![ok, err]));
        let result = self.resolve_expr_type(result, result_ty);
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
        (ok, TirExprKind::Try(ok, err, result))
      }
      ExprKind::Is(..) | ExprKind::LogicalOp(..) => {
        self.enter_scope();
        let kind = self._resolve_cond(expr)?;
        self.exit_scope();
        (self.bool(span), kind)
      }
      ExprKind::Paren(e) => self._resolve_expr(e)?,
      ExprKind::Error(e) => Err(*e)?,
      ExprKind::Hole => (self.types.new_var(span), TirExprKind::Hole),
      ExprKind::Deref(inner, _) => {
        let ty = self.types.new_var(span);
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        let inner = self.resolve_expr_type(inner, ref_ty);
        (ty, TirExprKind::Deref(inner))
      }
      ExprKind::Inverse(inner, _) => {
        let inner = self.resolve_expr(inner);
        (inner.ty.inverse(), TirExprKind::Inverse(inner))
      }
      ExprKind::Place(value, space) => {
        let value = self.resolve_expr(value);
        let space = self.resolve_expr(space);
        let ty = if self.types.unify(value.ty, space.ty).is_failure() {
          self.types.error(self.core.report(Diag::MismatchedValueSpaceTypes {
            span,
            value: self.types.show(self.chart, value.ty),
            space: self.types.show(self.chart, space.ty),
          }))
        } else {
          value.ty
        };
        (ty, TirExprKind::Place(value, space))
      }
      ExprKind::TupleField(tuple, index, _) => {
        let tuple = self.resolve_expr(tuple);
        let (ty, fields) = self.tuple_field(span, tuple.ty, *index).ok_or_else(|| {
          Diag::MissingTupleField { span, ty: self.types.show(self.chart, tuple.ty), i: *index }
        })?;
        (ty, TirExprKind::Field(tuple, *index, fields))
      }
      ExprKind::ObjectField(object, key) => {
        let object = self.resolve_expr(object);
        let (ty, index, fields) =
          self.object_field(span, object.ty, key.ident).ok_or_else(|| {
            Diag::MissingObjectField {
              span: key.span,
              ty: self.types.show(self.chart, object.ty),
              key: key.ident,
            }
          })?;
        (ty, TirExprKind::Field(object, index, fields))
      }
      ExprKind::Tuple(elements) => self._resolve_tuple(elements),
      ExprKind::Object(entries) => {
        let object = self.build_object(entries, Self::resolve_expr)?;
        let ty = self.types.new(TypeKind::Object(object.iter().map(|(&i, x)| (i, x.ty)).collect()));
        (ty, TirExprKind::Composite(object.into_values().collect()))
      }
      ExprKind::Unwrap(inner) => {
        let inner = self.resolve_expr(inner);

        let (struct_id, data_ty) = match self.types.force_kind(self.core, inner.ty) {
          (inv, TypeKind::Struct(struct_id, type_params)) => {
            let struct_id = *struct_id;
            let type_params = &type_params.clone();
            (struct_id, self.get_struct_data(span, inner.ty, struct_id, type_params).invert_if(inv))
          }
          _ => Err(Diag::UnwrapNonStruct { span })?,
        };

        (data_ty, TirExprKind::Unwrap(struct_id, inner))
      }
      ExprKind::Path(path, args) => {
        let mut args = args.as_ref();
        let (ty, kind) = 'inner: {
          if let Some(ident) = path.as_ident() {
            if let Some(bind) = self.scope.get(&ident).and_then(|x| x.last()) {
              break 'inner match bind.binding {
                Binding::Local(local, _, ty) => (ty, TirExprKind::Local(local)),
                Binding::Closure(id, ty) => (ty, TirExprKind::Closure(id)),
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
                  let (ty, kind) = self._resolve_tuple(args);
                  TirExpr { span, ty, kind: Box::new(kind) }
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
              (ty, TirExprKind::Struct(struct_id, data))
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
                  let (ty, kind) = self._resolve_tuple(args);
                  TirExpr { span, ty, kind: Box::new(kind) }
                }
              });
              let data = match data_ty {
                Some(data_ty) => Some(match data {
                  Some(data) => {
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
              (ty, TirExprKind::Enum(enum_id, variant_id, data))
            }
            Ok(DefValueKind::Const(const_id)) => {
              let (type_params, impl_params) =
                self.resolve_generics(path, self.chart.const_generics(const_id), true);
              let ty = self.types.import(self.sigs.const_sig(const_id), Some(&type_params)).ty;
              let rel = self.rels.consts.push((const_id, impl_params));
              (ty, TirExprKind::Const(rel))
            }
            Ok(DefValueKind::Fn(fn_id)) => {
              if let Some(args) = args.take() {
                let generics_id = self.chart.fn_generics(fn_id);
                let type_params_len = self.chart.generics[generics_id].type_params.len();
                let type_params = self.types.new_vars(path.span, type_params_len);
                let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
                if sig.params.len() != args.len() {
                  for arg in args {
                    _ = self.resolve_expr(arg);
                  }
                  Err(Diag::BadArgCount { span, expected: sig.params.len(), got: args.len() })?
                }
                let args = args.iter().map(|arg| self.resolve_expr(arg)).collect::<Vec<_>>();
                for (arg, ty) in args.iter().zip(sig.params) {
                  // just need inference; errors will be reported later
                  _ = self.types.unify(arg.ty, ty);
                }
                let generics = path.generics.as_ref();
                let (type_params, impl_params) =
                  self._resolve_generics(path.span, generics, generics_id, true, Some(type_params));
                let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
                for (arg, ty) in args.iter().zip(sig.params) {
                  self.expect_type(arg.span, arg.ty, ty);
                }
                let rel = self.rels.fns.push(FnRel::Item(fn_id, impl_params));
                (sig.ret_ty, TirExprKind::Call(rel, None, args))
              } else {
                _ = self.resolve_generics(path, GenericsId::NONE, true);
                (self.types.new(TypeKind::Fn(fn_id)), TirExprKind::Fn)
              }
            }
            Err(diag) => Err(diag)?,
          }
        };
        if let Some(args) = args {
          self._resolve_call(span, TirExpr { span, ty, kind: Box::new(kind) }, args)?
        } else {
          (ty, kind)
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

  fn tuple_field(&mut self, span: Span, ty: Type, index: usize) -> Option<(Type, Vec<Type>)> {
    match self.types.force_kind(self.core, ty) {
      (inv, TypeKind::Tuple(tuple)) => Some((tuple.get(index)?.invert_if(inv), tuple.clone())),
      (inv, TypeKind::Struct(struct_id, type_params)) => {
        let struct_id = *struct_id;
        let type_params = &type_params.clone();
        let data_ty = self.get_struct_data(span, ty, struct_id, type_params);
        self.tuple_field(span, data_ty.invert_if(inv), index)
      }
      (_, TypeKind::Error(_)) => Some((ty, vec![ty; index + 1])),
      _ => None,
    }
  }

  fn object_field(
    &mut self,
    span: Span,
    ty: Type,
    key: Ident<'core>,
  ) -> Option<(Type, usize, Vec<Type>)> {
    match self.types.force_kind(self.core, ty) {
      (inv, TypeKind::Object(entries)) => entries
        .iter()
        .enumerate()
        .find(|&(_, (&k, _))| k == key)
        .map(|(i, (_, t))| (t.invert_if(inv), i, entries.values().copied().collect())),
      (inv, TypeKind::Struct(struct_id, type_params)) => {
        let struct_id = *struct_id;
        let type_params = &type_params.clone();
        let data_ty = self.get_struct_data(span, ty, struct_id, type_params);
        self.object_field(span, data_ty.invert_if(inv), key)
      }
      (_, TypeKind::Error(_)) => Some((ty, 0, vec![ty])),
      _ => None,
    }
  }

  fn _resolve_tuple(&mut self, elements: &Vec<Expr<'core>>) -> (Type, TirExprKind) {
    let elements = elements.iter().map(|x| self.resolve_expr(x)).collect::<Vec<_>>();
    let ty = self.types.new(TypeKind::Tuple(elements.iter().map(|x| x.ty).collect()));
    (ty, TirExprKind::Composite(elements))
  }

  fn resolve_cond(&mut self, cond: &Expr<'core>) -> TirExpr {
    match self._resolve_cond(cond) {
      Ok(kind) => {
        let ty = self.bool(cond.span);
        TirExpr { span: cond.span, ty, kind: Box::new(kind) }
      }
      Err(diag) => self.error_expr(cond.span, diag),
    }
  }

  fn _resolve_cond(&mut self, cond: &Expr<'core>) -> Result<TirExprKind, Diag<'core>> {
    let span = cond.span;
    Ok(match &cond.kind {
      ExprKind::Error(err) => Err(*err)?,
      ExprKind::Is(expr, pat) => {
        let expr = self.resolve_expr(expr);
        let pat = self.resolve_pat_type(pat, expr.ty);
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
        *self.resolve_expr_type(cond, bool).kind
      }
    })
  }

  fn _resolve_call(
    &mut self,
    span: Span,
    func: TirExpr,
    args: &Vec<Expr<'core>>,
  ) -> Result<(Type, TirExprKind), Diag<'core>> {
    let args = args.iter().map(|arg| self.resolve_expr(arg)).collect::<Vec<_>>();
    let ret_ty = self.types.new_var(span);
    let impl_type = ImplType::Fn(func.ty, args.iter().map(|x| x.ty).collect(), ret_ty);
    let impl_ = self.find_impl(span, &impl_type);
    let rel = self.rels.fns.push(FnRel::Impl(impl_));
    Ok((ret_ty, TirExprKind::Call(rel, Some(func), args)))
  }

  fn resolve_range_expr(
    &mut self,
    span: Span,
    start: Option<&Expr<'core>>,
    end: Option<&Expr<'core>>,
    end_inclusive: bool,
  ) -> Result<(Type, TirExprKind), Diag<'core>> {
    let bound_value_ty = self.types.new_var(span);
    let left_bound = self.resolve_range_bound(span, bound_value_ty, start, true)?;
    let right_bound = self.resolve_range_bound(span, bound_value_ty, end, end_inclusive)?;
    let Some(range_id) = self.chart.builtins.range else {
      Err(Diag::MissingBuiltin { span, builtin: "Range" })?
    };
    let ty = self.types.new(TypeKind::Struct(range_id, vec![left_bound.ty, right_bound.ty]));
    Ok((
      ty,
      TirExprKind::Struct(
        range_id,
        TirExpr {
          span,
          ty: self.types.new(TypeKind::Tuple(vec![left_bound.ty, right_bound.ty])),
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
        let bound = self.resolve_expr_type(bound, bound_value_ty);
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
          kind: Box::new(TirExprKind::Struct(
            unbounded_id,
            TirExpr { span, ty: self.types.nil(), kind: Box::new(TirExprKind::Composite(vec![])) },
          )),
        })
      }
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
      Ok(self.rels.fns.push(FnRel::Item(fn_id, impl_params)))
    } else {
      Err(Diag::MissingBuiltin { span, builtin: name })?
    }
  }
}
