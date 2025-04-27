use std::collections::{BTreeMap, HashMap};

use vine_util::idx::{Counter, IdxVec};

use crate::{
  ast::{
    Block, Expr, ExprKind, Flex, GenericArgs, Ident, Impl, ImplKind, LogicalOp, Pat, PatKind, Span,
    Stmt, StmtKind, Trait, TraitKind, Ty, TyKind,
  },
  chart::{Builtins, Chart, DefId, GenericsId, PatternDefKind, TypeDefId, ValueDefId},
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  finder::find_impl,
  signatures::{Signatures, ValueSigKind},
  tir::{
    ClosureId, LabelId, Local, TirBlock, TirBlockKind, TirClosure, TirExpr, TirExprKind, TirImpl,
    TirPat, TirPatKind,
  },
  types::{ImplType, Type, TypeKind, Types},
};

mod resolve_path;

#[derive(Debug)]
pub struct Resolver<'core, 'ctx> {
  pub core: &'core Core<'core>,
  pub chart: &'ctx mut Chart<'core>,
  pub sigs: &'ctx mut Signatures<'core>,
  pub types: Types<'core>,

  return_ty: Option<Type>,
  labels_by_name: HashMap<Ident<'core>, Vec<LabelId>>,
  loops: Vec<LabelId>,
  labels: IdxVec<LabelId, LabelInfo>,
  closures: IdxVec<ClosureId, TirClosure<'core>>,
  cur_value_def: ValueDefId,
  cur_def: DefId,
  cur_generics: GenericsId,
  bindings: HashMap<Ident<'core>, Vec<(usize, Binding)>>,
  scope_depth: usize,
  locals: Counter<Local>,
}

#[derive(Debug, Clone, Copy)]
enum Binding {
  Local(Local, Type),
  Closure(ClosureId, Type),
}

#[derive(Debug, Clone, Copy)]
struct LabelInfo {
  break_ty: Type,
  is_loop: bool,
}

impl<'core> Resolver<'core, '_> {
  pub fn resolve_expr_ty(&mut self, expr: &Expr<'core>, ty: Type) -> TirExpr<'core> {
    let expr = self.resolve_expr(expr);
    if !self.types.unify(ty, expr.ty).is_ok() {
      self.core.report(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.types.show(self.chart, ty),
        found: self.types.show(self.chart, expr.ty),
      });
    }
    expr
  }

  fn resolve_expr(&mut self, expr: &Expr<'core>) -> TirExpr<'core> {
    let (ty, kind) = self._resolve_expr(expr).unwrap_or_else(|diag| {
      let err = self.core.report(diag);
      (self.types.error(err), TirExprKind::Error(err))
    });
    TirExpr { span: expr.span, ty, kind }
  }

  fn _resolve_expr(
    &mut self,
    expr: &Expr<'core>,
  ) -> Result<(Type, TirExprKind<'core>), Diag<'core>> {
    let span = expr.span;
    Ok(match &expr.kind {
      ExprKind::Hole => (self.types.new_var(), TirExprKind::Hole),
      ExprKind::Paren(inner) => self._resolve_expr(inner)?,
      ExprKind::Path(path, args) => {
        if let Some(ident) = path.as_ident() {
          if let Some(&(_, binding)) = self.bindings.get(&ident).and_then(|x| x.last()) {
            let (ty, kind) = match binding {
              Binding::Local(local, ty) => (ty, TirExprKind::Local(local)),
              Binding::Closure(id, ty) => (ty, TirExprKind::Closure(id)),
            };
            if let Some(args) = args {
              return Ok(self._resolve_call(span, TirExpr { span, ty, kind }, args));
            } else {
              return Ok((ty, kind));
            }
          }
        }
        let value_id = self.resolve_path_to(self.cur_def, path, "value", |d| d.value_def)?;
        let generics = self.chart.values[value_id].generics;
        let (type_params, impl_params) = self.resolve_generic_args(generics, &path.generics);
        let sig = &self.sigs.values[value_id];
        let mut import = self.types.import(&sig.types, Some(&type_params));
        match &sig.kind {
          ValueSigKind::Const(ty) => {
            let ty = import.transfer(*ty);
            let kind = TirExprKind::Def(value_id, impl_params);
            if let Some(args) = args {
              self._resolve_call(span, TirExpr { span, ty, kind }, args)
            } else {
              (ty, kind)
            }
          }
          ValueSigKind::Fn(params, ret) => {
            if let Some(args) = args {
              let params = params.iter().map(|&t| import.transfer(t)).collect::<Vec<_>>();
              let ret = import.transfer(*ret);
              let args = args
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                  let ty = params.get(i).copied().unwrap_or_else(|| self.types.new_var());
                  self.resolve_expr_ty(arg, ty)
                })
                .collect::<Vec<_>>();
              if args.len() != params.len() {
                Err(Diag::BadArgCount { span, expected: params.len(), got: args.len() })?
              }
              (
                ret,
                TirExprKind::Call(
                  TirImpl::ClosureFn(value_id, ClosureId(0), impl_params.clone()),
                  None,
                  args,
                ),
              )
            } else {
              let params = params.iter().map(|&t| import.transfer(t)).collect();
              let ret = import.transfer(*ret);
              let ty = self.types.new(TypeKind::Fn(
                value_id,
                ClosureId(0),
                type_params,
                impl_params.clone(),
                Flex::Full,
                params,
                ret,
              ));
              (ty, TirExprKind::Def(value_id, impl_params))
            }
          }
          &ValueSigKind::Struct(struct_id, data_ty) => {
            let data_ty = import.transfer(data_ty);
            let Some(args) = args else { Err(Diag::ExpectedDataExpr { span })? };
            let [data] = &**args else { Err(Diag::ConstructorMultiArgs { span })? };
            let data = self.resolve_expr_ty(data, data_ty);
            (
              self.types.new(TypeKind::Struct(struct_id, type_params)),
              TirExprKind::Struct(struct_id, Box::new(data)),
            )
          }
          &ValueSigKind::Enum(enum_id, variant_id, variant_data) => {
            let data = match (variant_data, args) {
              (None, None) => None,
              (Some(ty), Some(args)) => {
                let [data] = &**args else { Err(Diag::ConstructorMultiArgs { span })? };
                Some(Box::new(self.resolve_expr_ty(data, ty)))
              }
              (None, Some(_)) => Err(Diag::EnumVariantNoData { span })?,
              (Some(_), None) => Err(Diag::ExpectedDataExpr { span })?,
            };
            (
              self.types.new(TypeKind::Enum(enum_id, type_params)),
              TirExprKind::Enum(enum_id, variant_id, data),
            )
          }
        }
      }
      ExprKind::Assign(inv, space, value) => {
        let space = self.resolve_expr(space);
        let value = self.resolve_expr_ty(value, space.ty);
        (self.types.nil(), TirExprKind::Assign(*inv, Box::new(space), Box::new(value)))
      }
      ExprKind::Match(expr, arms) => {
        let expr = self.resolve_expr(expr);
        let result = self.types.new_var();
        let arms = arms
          .iter()
          .map(|(pat, block)| {
            self.push_scope();
            let pat = self.resolve_pat_ty(pat, expr.ty);
            let block = self.resolve_block_ty(block, result);
            self.pop_scope();
            (pat, block)
          })
          .collect();
        (result, TirExprKind::Match(Box::new(expr), arms))
      }
      ExprKind::If(then_branches, else_branch) => {
        let bool = self.require_builtin_ty(span, "Bool", |b| b.bool)?;
        let result = if else_branch.is_some() { self.types.new_var() } else { self.types.nil() };
        let then_branches = then_branches
          .iter()
          .map(|(cond, block)| {
            self.push_scope();
            let cond = self.resolve_cond(bool, cond);
            let block = self.resolve_block_ty(block, result);
            self.pop_scope();
            (cond, block)
          })
          .collect();
        let else_branch = else_branch.as_ref().map(|block| self.resolve_block_ty(block, result));
        (result, TirExprKind::If(then_branches, else_branch))
      }
      ExprKind::Do(label, block) => {
        let result = self.types.new_var();
        self.bind_label(*label, LabelInfo { break_ty: result, is_loop: false }, |self_, id| {
          let block = self_.resolve_block_ty(block, result);
          (result, TirExprKind::Do(id, block))
        })
      }
      ExprKind::While(label, cond, block) => {
        let nil = self.types.nil();
        let bool = self.require_builtin_ty(span, "Bool", |b| b.bool)?;
        self.bind_label(*label, LabelInfo { break_ty: nil, is_loop: true }, |self_, id| {
          self_.push_scope();
          let cond = self_.resolve_cond(bool, cond);
          let block = self_.resolve_block_ty(block, nil);
          self_.pop_scope();
          (nil, TirExprKind::While(id, Box::new(cond), block))
        })
      }
      ExprKind::Loop(label, block) => {
        let nil = self.types.nil();
        let result = self.types.new_var();
        self.bind_label(*label, LabelInfo { break_ty: result, is_loop: true }, |self_, id| {
          let block = self_.resolve_block_ty(block, nil);
          (result, TirExprKind::Loop(id, block))
        })
      }
      ExprKind::Fn(flex, params, ret, body) => {
        let params = params.iter().map(|p| self.resolve_pat(p)).collect::<Vec<_>>();
        let ret = ret.as_ref().map(|t| self.resolve_ty(t)).unwrap_or_else(|| self.types.new_var());
        let body = self.resolve_block_ty(body, ret);
        let param_tys = params.iter().map(|p| p.ty).collect();
        let id = self.closures.push(TirClosure { flex: *flex, params, body });
        (self.fn_type(id, *flex, param_tys, ret), TirExprKind::Closure(id))
      }
      ExprKind::Return(value) => {
        if let Some(ty) = self.return_ty {
          let nil = self.types.nil();
          if let Some(value) = value {
            let value = self.resolve_expr_ty(value, ty);
            (self.types.new_var(), TirExprKind::Return(Some(Box::new(value))))
          } else if self.types.unify(ty, nil).is_ok() {
            (self.types.new_var(), TirExprKind::Return(None))
          } else {
            Err(Diag::MissingReturnExpr { span, ty: self.types.show(self.chart, ty) })?
          }
        } else {
          Err(Diag::NoReturn { span })?
        }
      }
      ExprKind::Break(label, value) => {
        let id = if let &Some(label) = label {
          if let Some(&id) = self.labels_by_name.get(&label).and_then(|x| x.last()) {
            id
          } else {
            Err(Diag::UnboundLabel { span, label })?
          }
        } else if let Some(&id) = self.loops.last() {
          id
        } else {
          Err(Diag::NoLoopBreak { span })?
        };
        let ty = self.labels[id].break_ty;
        let nil = self.types.nil();
        if let Some(value) = value {
          let value = self.resolve_expr_ty(value, ty);
          (self.types.new_var(), TirExprKind::Break(id, Some(Box::new(value))))
        } else if self.types.unify(ty, nil).is_ok() {
          (self.types.new_var(), TirExprKind::Break(id, None))
        } else {
          Err(Diag::MissingBreakExpr { span, ty: self.types.show(self.chart, ty) })?
        }
      }
      ExprKind::Continue(label) => {
        if let &Some(label) = label {
          if let Some(&id) = self.labels_by_name.get(&label).and_then(|x| x.last()) {
            if self.labels[id].is_loop {
              (self.types.new_var(), TirExprKind::Continue(id))
            } else {
              Err(Diag::NoContinueLabel { span, label })?
            }
          } else {
            Err(Diag::UnboundLabel { span, label })?
          }
        } else if let Some(&id) = self.loops.last() {
          (self.types.new_var(), TirExprKind::Continue(id))
        } else {
          Err(Diag::NoLoopContinue { span })?
        }
      }
      ExprKind::Ref(inner, _) => {
        let inner = self.resolve_expr(inner);
        (self.types.new(TypeKind::Ref(inner.ty)), TirExprKind::Ref(Box::new(inner)))
      }
      ExprKind::Deref(inner, _) => {
        let ty = self.types.new_var();
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        let inner = self.resolve_expr_ty(inner, ref_ty);
        (ty, TirExprKind::Deref(Box::new(inner)))
      }
      ExprKind::Inverse(inner, _) => {
        let inner = self.resolve_expr(inner);
        (self.types.inverse(inner.ty), TirExprKind::Inverse(Box::new(inner)))
      }
      ExprKind::Place(value, space) => {
        let value = self.resolve_expr(value);
        let space = self.resolve_expr(space);
        let ty = if self.types.unify(value.ty, space.ty).is_ok() {
          value.ty
        } else {
          self.types.error(self.core.report(Diag::MismatchedValueSpaceTypes {
            span,
            value: self.types.show(self.chart, value.ty),
            space: self.types.show(self.chart, space.ty),
          }))
        };
        (ty, TirExprKind::Place(Box::new(value), Box::new(space)))
      }
      ExprKind::Tuple(els) => {
        let (tys, els) = els.iter().map(|e| self.resolve_expr(e)).map(|e| (e.ty, e)).collect();
        (self.types.new(TypeKind::Tuple(tys)), TirExprKind::Composite(els))
      }
      ExprKind::Object(entries) => {
        let (tys, els) = entries
          .iter()
          .map(|(k, e)| (k.ident, self.resolve_expr(e)))
          .map(|(k, e)| ((k, e.ty), e))
          .collect();
        (self.types.new(TypeKind::Object(tys)), TirExprKind::Composite(els))
      }
      ExprKind::List(els) => {
        let list = self.require_builtin(span, "List", |b| b.list)?;
        let el_ty = self.types.new_var();
        let els = els.iter().map(|e| self.resolve_expr_ty(e, el_ty)).collect();
        (self.types.new(TypeKind::Struct(list, vec![el_ty])), TirExprKind::List(els))
      }
      ExprKind::TupleField(tuple, index) => {
        let tuple = self.resolve_expr(tuple);
        let (len, ty) = self.resolve_tuple_field(span, tuple.ty, *index)?;
        (ty, TirExprKind::Field(Box::new(tuple), *index, len))
      }
      ExprKind::ObjectField(object, key) => {
        let object = self.resolve_expr(object);
        let (index, len, ty) = self.resolve_object_field(span, object.ty, key.ident)?;
        (ty, TirExprKind::Field(Box::new(object), index, len))
      }
      ExprKind::Unwrap(struct_) => {
        let struct_ = self.resolve_expr(struct_);
        let Some(TypeKind::Struct(struct_id, type_params)) = self.types.kind(struct_.ty) else {
          Err(Diag::UnwrapNonStruct { span })?
        };
        let sig = &self.sigs.structs[*struct_id];
        let type_params = type_params.clone();
        let ty = self.types.import(&sig.types, Some(&type_params)).transfer(sig.data);
        (ty, TirExprKind::Unwrap(Box::new(struct_)))
      }
      ExprKind::Method(expr, ident, generics, exprs) => todo!(),
      ExprKind::Call(func, args) => {
        let func = self.resolve_expr(func);
        self._resolve_call(span, func, args)
      }
      ExprKind::Neg(value) => {
        let value = self.resolve_expr(value);
        let out = self.types.new_var();
        let call =
          self.resolve_builtin_fn(span, self.chart.builtins.neg, vec![value.ty, out], vec![value]);
        (out, call)
      }
      ExprKind::BinaryOp(op, lhs, rhs) => {
        let lhs = self.resolve_expr(lhs);
        let rhs = self.resolve_expr(rhs);
        let out = self.types.new_var();
        let call = self.resolve_builtin_fn(
          span,
          self.chart.builtins.binary_ops[op],
          vec![lhs.ty, rhs.ty, out],
          vec![lhs, rhs],
        );
        (out, call)
      }
      ExprKind::Not(value) => {
        let value = self.resolve_expr(value);
        let out = self.types.new_var();
        let call =
          self.resolve_builtin_fn(span, self.chart.builtins.not, vec![value.ty, out], vec![value]);
        (out, call)
      }
      ExprKind::Is(..) | ExprKind::LogicalOp(..) => {
        let bool = self.require_builtin_ty(span, "Bool", |b| b.bool)?;
        (bool, self._resolve_cond(bool, expr))
      }
      ExprKind::ComparisonOp(expr, items) => todo!(),
      ExprKind::BinaryOpAssign(binary_op, expr, expr1) => todo!(),
      ExprKind::Cast(value, ty, _) => {
        let value = self.resolve_expr(value);
        let ty = self.resolve_ty(ty);
        let call =
          self.resolve_builtin_fn(span, self.chart.builtins.cast, vec![value.ty, ty], vec![value]);
        (ty, call)
      }
      ExprKind::Bool(value) => {
        (self.require_builtin_ty(span, "Bool", |b| b.bool)?, TirExprKind::Bool(*value))
      }
      ExprKind::N32(value) => {
        (self.require_builtin_ty(span, "N32", |b| b.n32)?, TirExprKind::N32(*value))
      }
      ExprKind::F32(value) => {
        (self.require_builtin_ty(span, "F32", |b| b.f32)?, TirExprKind::F32(*value))
      }
      ExprKind::Char(value) => {
        (self.require_builtin_ty(span, "Char", |b| b.char)?, TirExprKind::Char(*value))
      }
      ExprKind::String(init, rest) => {
        let string = self.require_builtin(span, "String", |b| b.string)?;
        let string = self.types.new(TypeKind::Struct(string, vec![]));
        let rest = rest
          .iter()
          .map(|(value, seg)| {
            let span = value.span;
            let value = self.resolve_expr(value);
            let call = self.resolve_builtin_fn(
              span,
              self.chart.builtins.cast,
              vec![value.ty, string],
              vec![value],
            );
            (TirExpr { span, ty: string, kind: call }, seg.content.clone())
          })
          .collect();
        (string, TirExprKind::String(init.content.clone(), rest))
      }
      ExprKind::InlineIvy(binds, ty, _, net) => {
        let binds = binds.iter().map(|(n, d, e)| (*n, *d, self.resolve_expr(e))).collect();
        let ty = self.resolve_ty(ty);
        (ty, TirExprKind::InlineIvy(binds, net.clone()))
      }
      ExprKind::Error(err) => Err(*err)?,
    })
  }

  fn _resolve_call(
    &mut self,
    span: Span,
    func: TirExpr<'core>,
    args: &Vec<Expr<'core>>,
  ) -> (Type, TirExprKind<'core>) {
    let (arg_tys, args) = args.iter().map(|e| self.resolve_expr(e)).map(|e| (e.ty, e)).collect();
    let ret = self.types.new_var();
    let impl_ty = ImplType::Fn(func.ty, arg_tys, ret);
    let impl_ = self.find_impl(span, &impl_ty);
    (ret, TirExprKind::Call(impl_, Some(Box::new(func)), args))
  }

  fn fn_type(&mut self, id: ClosureId, flex: Flex, params: Vec<Type>, ret: Type) -> Type {
    self.types.new(TypeKind::Fn(self.cur_value_def, id, todo!(), todo!(), flex, params, ret))
  }

  fn resolve_cond(&mut self, bool: Type, expr: &Expr<'core>) -> TirExpr<'core> {
    TirExpr { span: expr.span, ty: bool, kind: self._resolve_cond(bool, expr) }
  }

  fn _resolve_cond(&mut self, bool: Type, expr: &Expr<'core>) -> TirExprKind<'core> {
    match &expr.kind {
      ExprKind::LogicalOp(op, a, b) => {
        let (a, b) = match op {
          LogicalOp::And => {
            let a = self.resolve_cond(bool, a);
            let b = self.resolve_cond(bool, b);
            (a, b)
          }
          LogicalOp::Or => {
            self.push_scope();
            let a = self.resolve_cond(bool, a);
            self.pop_scope();
            self.push_scope();
            let b = self.resolve_cond(bool, b);
            self.pop_scope();
            (a, b)
          }
          LogicalOp::Implies => {
            self.push_scope();
            let a = self.resolve_cond(bool, a);
            let b = self.resolve_cond(bool, b);
            self.pop_scope();
            (a, b)
          }
        };
        TirExprKind::LogicalOp(*op, Box::new(a), Box::new(b))
      }
      ExprKind::Is(expr, pat) => {
        let expr = self.resolve_expr(expr);
        let pat = self.resolve_pat_ty(pat, expr.ty);
        TirExprKind::Is(Box::new(expr), Box::new(pat))
      }
      _ => self.resolve_expr_ty(expr, bool).kind,
    }
  }

  fn resolve_pat_ty(&mut self, pat: &Pat<'core>, ty: Type) -> TirPat {
    let pat = self.resolve_pat(pat);
    if !self.types.unify(ty, pat.ty).is_ok() {
      self.core.report(Diag::ExpectedTypeFound {
        span: pat.span,
        expected: self.types.show(self.chart, ty),
        found: self.types.show(self.chart, pat.ty),
      });
    }
    pat
  }

  fn resolve_pat(&mut self, pat: &Pat<'core>) -> TirPat {
    let (ty, kind) = self._resolve_pat(pat).unwrap_or_else(|diag| {
      let err = self.core.report(diag);
      (self.types.error(err), TirPatKind::Error(err))
    });
    TirPat { span: pat.span, ty, kind }
  }

  fn _resolve_pat(&mut self, pat: &Pat<'core>) -> Result<(Type, TirPatKind), Diag<'core>> {
    let span = pat.span;
    Ok(match &pat.kind {
      PatKind::Hole => (self.types.new_var(), TirPatKind::Hole),
      PatKind::Paren(inner) => self._resolve_pat(inner)?,
      PatKind::Annotation(inner, ty) => {
        let ty = self.resolve_ty(ty);
        let inner = self.resolve_pat_ty(inner, ty);
        (ty, inner.kind)
      }
      PatKind::Path(path, data) => {
        match self.resolve_path_to(self.cur_def, path, "value", |d| d.pattern_def) {
          Ok(pat_id) => {
            let pat = &self.chart.patterns[pat_id];
            match pat.kind {
              PatternDefKind::Struct(struct_id) => {
                let (params, _) = self.resolve_generic_args(pat.generics, &path.generics);
                let sig = &self.sigs.structs[struct_id];
                let Some(data) = data else { Err(Diag::ExpectedDataSubpattern { span })? };
                let data_ty = self.types.import(&sig.types, Some(&params)).transfer(sig.data);
                let data = self.resolve_pat_ty(data, data_ty);
                (
                  self.types.new(TypeKind::Struct(struct_id, params)),
                  TirPatKind::Struct(struct_id, Box::new(data)),
                )
              }
              PatternDefKind::Enum(enum_id, variant_id) => {
                let (params, _) = self.resolve_generic_args(pat.generics, &path.generics);
                let sig = &self.sigs.enums[enum_id];
                let variant_data = sig.variant_data[variant_id];
                let data = match (variant_data, data) {
                  (None, None) => None,
                  (Some(ty), Some(data)) => Some(Box::new(self.resolve_pat_ty(data, ty))),
                  (None, Some(_)) => Err(Diag::EnumVariantNoData { span })?,
                  (Some(_), None) => Err(Diag::ExpectedDataSubpattern { span })?,
                };
                (
                  self.types.new(TypeKind::Enum(enum_id, params)),
                  TirPatKind::Enum(enum_id, variant_id, data),
                )
              }
            }
          }
          Err(diag) => {
            if data.is_none() {
              if let Some(ident) = path.as_ident() {
                let local = self.locals.next();
                let ty = self.types.new_var();
                self.bind(ident, Binding::Local(local, ty));
                return Ok((ty, TirPatKind::Local(local)));
              }
            }
            Err(diag)?
          }
        }
      }
      PatKind::Ref(inner) => {
        let inner = self.resolve_pat(inner);
        (self.types.new(TypeKind::Ref(inner.ty)), TirPatKind::Ref(Box::new(inner)))
      }
      PatKind::Deref(inner) => {
        let ty = self.types.new_var();
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        let inner = self.resolve_pat_ty(inner, ref_ty);
        (self.types.inverse(inner.ty), TirPatKind::Deref(Box::new(inner)))
      }
      PatKind::Inverse(inner) => {
        let inner = self.resolve_pat(inner);
        (self.types.inverse(inner.ty), TirPatKind::Inverse(Box::new(inner)))
      }
      PatKind::Tuple(els) => {
        let (tys, els) = els.iter().map(|e| self.resolve_pat(e)).map(|e| (e.ty, e)).collect();
        (self.types.new(TypeKind::Tuple(tys)), TirPatKind::Composite(els))
      }
      PatKind::Object(entries) => {
        let (ty, els) = self.resolve_object(span, entries, |self_, (key, value)| {
          let value = self_.resolve_pat(value);
          (key.ident, value.ty, value)
        })?;
        (ty, TirPatKind::Composite(els))
      }
      PatKind::Error(err) => Err(*err)?,
    })
  }

  fn require_builtin<T>(
    &self,
    span: Span,
    name: &'static str,
    get: impl FnOnce(&Builtins) -> Option<T>,
  ) -> Result<T, ErrorGuaranteed> {
    if let Some(builtin) = get(&self.chart.builtins) {
      Ok(builtin)
    } else {
      Err(self.core.report(Diag::MissingBuiltin { span, builtin: name }))
    }
  }

  fn require_builtin_ty(
    &mut self,
    span: Span,
    name: &'static str,
    get: impl FnOnce(&Builtins) -> Option<TypeDefId>,
  ) -> Result<Type, ErrorGuaranteed> {
    if let Some(builtin) = get(&self.chart.builtins) {
      Ok(self.types.new(TypeKind::Opaque(builtin)))
    } else {
      Err(self.core.report(Diag::MissingBuiltin { span, builtin: name }))
    }
  }

  fn resolve_ty(&mut self, ty: &Ty<'core>) -> Type {
    let span = ty.span;
    match &ty.kind {
      TyKind::Hole => self.types.new_var(),
      TyKind::Paren(inner) => self.resolve_ty(inner),
      TyKind::Tuple(els) => {
        let els = els.iter().map(|t| self.resolve_ty(t)).collect();
        self.types.new(TypeKind::Tuple(els))
      }
      TyKind::Object(entries) => self
        .resolve_object(span, entries, |self_, (key, ty)| (key.ident, self_.resolve_ty(ty), ()))
        .map(|x| x.0)
        .unwrap_or_else(|err| self.types.error(err)),
      TyKind::Ref(inner) => {
        let inner = self.resolve_ty(inner);
        self.types.new(TypeKind::Ref(inner))
      }
      TyKind::Inverse(inner) => {
        let inner = self.resolve_ty(inner);
        self.types.inverse(inner)
      }
      TyKind::Path(path) => todo!(),
      TyKind::Error(err) => self.types.error(*err),
    }
  }

  fn push_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn pop_scope(&mut self) {
    self.scope_depth -= 1;
    for bindings in self.bindings.values_mut() {
      while bindings.last_mut().is_some_and(|x| x.0 > self.scope_depth) {
        bindings.pop();
      }
    }
  }

  fn bind(&mut self, ident: Ident<'core>, binding: Binding) {
    self.bindings.entry(ident).or_default().push((self.scope_depth, binding));
  }

  fn resolve_block_ty(&mut self, block: &Block<'core>, ty: Type) -> TirBlock<'core> {
    self.resolve_stmts_ty(block.span, &block.stmts, ty)
  }

  fn resolve_stmts_ty(&mut self, span: Span, stmts: &[Stmt<'core>], ty: Type) -> TirBlock<'core> {
    TirBlock { span, ty, kind: self._resolve_stmts_ty(span, stmts, ty) }
  }

  fn _resolve_stmts_ty(
    &mut self,
    span: Span,
    stmts: &[Stmt<'core>],
    ty: Type,
  ) -> TirBlockKind<'core> {
    let [stmt, stmts @ ..] = stmts else {
      let nil = self.types.nil();
      return if self.types.unify(ty, nil).is_ok() {
        TirBlockKind::Nil
      } else {
        TirBlockKind::Error(
          self.core.report(Diag::MissingBlockExpr { span, ty: self.types.show(self.chart, ty) }),
        )
      };
    };
    match &stmt.kind {
      StmtKind::Let(l) => {
        let init = l.init.as_ref().map(|i| self.resolve_expr(i));
        let else_block = l.else_block.as_ref().map(|b| self.resolve_block_ty(b, ty));
        let bind = self.resolve_pat(&l.bind);
        if let Some(init) = &init {
          if !self.types.unify(bind.ty, init.ty).is_ok() {
            self.core.report(Diag::ExpectedTypeFound {
              span: init.span,
              expected: self.types.show(self.chart, bind.ty),
              found: self.types.show(self.chart, init.ty),
            });
          }
        }
        if let Some(else_block) = else_block {
          TirBlockKind::LetElse(
            bind,
            Box::new(init.unwrap()),
            Box::new(else_block),
            Box::new(self.resolve_stmts_ty(span, stmts, ty)),
          )
        } else {
          TirBlockKind::Let(
            bind,
            init.map(Box::new),
            Box::new(self.resolve_stmts_ty(span, stmts, ty)),
          )
        }
      }
      StmtKind::LetFn(l) => {
        let params = l.params.iter().map(|p| self.resolve_pat(p)).collect::<Vec<_>>();
        let ret =
          l.ret.as_ref().map(|t| self.resolve_ty(t)).unwrap_or_else(|| self.types.new_var());
        let body = self.resolve_block_ty(&l.body, ret);
        let param_tys = params.iter().map(|p| p.ty).collect();
        let id = self.closures.push(TirClosure { flex: l.flex, params, body });
        let ty = self.fn_type(id, l.flex, param_tys, ret);
        self.bind(l.name, Binding::Closure(id, ty));
        self._resolve_stmts_ty(span, stmts, ty)
      }
      StmtKind::Expr(expr, semi) => {
        if !semi && stmts.is_empty() {
          let expr = self.resolve_expr_ty(expr, ty);
          TirBlockKind::Expr(Box::new(expr))
        } else {
          let expr = self.resolve_expr_ty(expr, ty);
          TirBlockKind::Seq(Box::new(expr), Box::new(self.resolve_stmts_ty(span, stmts, ty)))
        }
      }
      StmtKind::Item(_) | StmtKind::Empty => self._resolve_stmts_ty(span, stmts, ty),
    }
  }

  fn resolve_tuple_field(
    &mut self,
    span: Span,
    mut ty: Type,
    index: usize,
  ) -> Result<(usize, Type), ErrorGuaranteed> {
    loop {
      break match self.types.kind(ty) {
        Some(TypeKind::Tuple(els)) => {
          if index < els.len() {
            return Ok((els.len(), els[index]));
          }
        }
        Some(TypeKind::Struct(adt_id, type_params)) => {
          let sig = &self.sigs.structs[*adt_id];
          ty = self.types.import(&sig.types, Some(&type_params.clone())).transfer(sig.data);
          continue;
        }
        Some(TypeKind::Error(err)) => return Err(*err),
        _ => {}
      };
    }
    Err(self.core.report(Diag::MissingTupleField {
      span,
      ty: self.types.show(self.chart, ty),
      index,
    }))
  }

  fn resolve_object_field(
    &mut self,
    span: Span,
    mut ty: Type,
    key: Ident<'core>,
  ) -> Result<(usize, usize, Type), ErrorGuaranteed> {
    loop {
      break match self.types.kind(ty) {
        Some(TypeKind::Object(entries)) => {
          if let Some((index, (_, &ty))) = entries.iter().enumerate().find(|&(_, (&k, _))| k == key)
          {
            return Ok((index, entries.len(), ty));
          }
        }
        Some(TypeKind::Struct(adt_id, type_params)) => {
          let sig = &self.sigs.structs[*adt_id];
          ty = self.types.import(&sig.types, Some(&type_params.clone())).transfer(sig.data);
          continue;
        }
        Some(TypeKind::Error(err)) => return Err(*err),
        _ => {}
      };
    }
    Err(self.core.report(Diag::MissingObjectField {
      span,
      ty: self.types.show(self.chart, ty),
      key,
    }))
  }

  fn bind_label<T>(
    &mut self,
    label: Option<Ident<'core>>,
    info: LabelInfo,
    f: impl FnOnce(&mut Self, LabelId) -> T,
  ) -> T {
    let id = self.labels.push(info);
    if let Some(label) = label {
      self.labels_by_name.entry(label).or_default().push(id);
    }
    if info.is_loop {
      self.loops.push(id);
    }
    let result = f(self, id);
    if let Some(label) = label {
      self.labels_by_name.get_mut(&label).unwrap().pop();
    }
    if info.is_loop {
      self.loops.pop();
    }
    result
  }

  fn resolve_object<I, T>(
    &mut self,
    span: Span,
    iter: impl IntoIterator<Item = I>,
    mut f: impl FnMut(&mut Self, I) -> (Ident<'core>, Type, T),
  ) -> Result<(Type, Vec<T>), ErrorGuaranteed> {
    let mut map = BTreeMap::new();
    let mut err = None;
    for i in iter {
      let (key, ty, t) = f(self, i);
      let old = map.insert(key, (ty, t));
      if old.is_some() {
        err = Some(self.core.report(Diag::DuplicateKey { span }));
      }
    }
    if let Some(err) = err {
      Err(err)
    } else {
      let (tys, ts) = map.into_iter().map(|(key, (ty, t))| ((key, ty), t)).collect();
      Ok((self.types.new(TypeKind::Object(tys)), ts))
    }
  }

  fn resolve_builtin_fn(
    &self,
    span: Span,
    op: Option<ValueDefId>,
    type_args: Vec<Type>,
    args: Vec<TirExpr<'core>>,
  ) -> TirExprKind<'core> {
    todo!()
  }

  fn find_impl(&mut self, span: Span, impl_ty: &ImplType) -> TirImpl {
    find_impl(
      &mut self.types,
      self.core,
      self.chart,
      self.sigs,
      self.cur_def,
      self.cur_generics,
      span,
      impl_ty,
    )
  }

  fn resolve_generic_args(
    &mut self,
    generics: GenericsId,
    args: &Option<GenericArgs>,
  ) -> (Vec<Type>, Vec<TirImpl>) {
    todo!()
  }

  fn resolve_trait(&mut self, trait_: &Trait<'core>) -> ImplType {
    self._resolve_trait(trait_).unwrap_or_else(|err| ImplType::Error(self.core.report(err)))
  }

  fn _resolve_trait(&mut self, trait_: &Trait<'core>) -> Result<ImplType, Diag<'core>> {
    Ok(match &trait_.kind {
      TraitKind::Fn(recv, params, ret) => {
        let recv = self.resolve_ty(recv);
        let params = params.iter().map(|p| self.resolve_ty(p)).collect();
        let ret = ret.as_ref().map(|r| self.resolve_ty(r)).unwrap_or(self.types.nil());
        ImplType::Fn(recv, params, ret)
      }
      TraitKind::Path(path) => {
        let trait_id = self.resolve_path_to(self.cur_def, path, "trait", |d| d.trait_def)?;
        let generics = self.chart.traits[trait_id].generics;
        let (type_params, _) = self.resolve_generic_args(generics, &path.generics);
        ImplType::Trait(trait_id, type_params)
      }
      TraitKind::Error(e) => Err(*e)?,
    })
  }

  fn resolve_impl(&mut self, impl_: &Impl<'core>, ty: ImplType) -> TirImpl {
    self._resolve_impl(impl_, ty).unwrap_or_else(|err| TirImpl::Error(self.core.report(err)))
  }

  fn _resolve_impl(&mut self, impl_: &Impl<'core>, ty: ImplType) -> Result<TirImpl, Diag<'core>> {
    let span = impl_.span;
    Ok(match &impl_.kind {
      ImplKind::Hole => self.find_impl(impl_.span, &ty),
      ImplKind::Path(path) => {
        let impl_id = self.resolve_path_to(self.cur_def, path, "impl", |d| d.impl_def)?;
        let generics = self.chart.impls[impl_id].generics;
        let (type_params, impl_params) = self.resolve_generic_args(generics, &path.generics);
        let sig = &self.sigs.impls[impl_id];
        let impl_ty = self.types.import(&sig.types, Some(&type_params)).transfer_impl_type(&sig.ty);
        if !self.types.unify_impl_type(&impl_ty, &ty).is_ok() {
          Err(Diag::ExpectedTypeFound {
            span,
            expected: self.types.show_impl_type(self.chart, &ty),
            found: self.types.show_impl_type(self.chart, &impl_ty),
          })?
        }
        TirImpl::Def(impl_id, impl_params)
      }
      ImplKind::Error(e) => Err(*e)?,
    })
  }
}
