use crate::{
  ast::{Block, Expr, ExprKind, Ident, LogicalOp, Pat, PatKind, Span, Ty},
  chart::{Builtins, Chart, TypeDefId},
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  tir::{TirBlock, TirExpr, TirExprKind, TirPat, TirPatKind},
  types::{Type, TypeKind, Types},
};

mod resolve_path;

#[derive(Debug)]
pub struct Resolver<'core, 'ctx> {
  pub core: &'core Core<'core>,
  pub chart: &'ctx mut Chart<'core>,
  pub types: Types<'core>,

  return_ty: Option<Type>,
}

impl<'core> Resolver<'core, '_> {
  fn resolve_expr_ty(&mut self, expr: &Expr<'core>, ty: Type) -> TirExpr<'core> {
    let expr = self.resolve_expr(expr);
    if !self.types.unify(ty, expr.ty) {
      self.core.report(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.types.show(ty),
        found: self.types.show(expr.ty),
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
      ExprKind::Path(path) => {
        let base = todo!();
        let value_def_id = self.resolve_path_to(base, path, "value", |d| d.value_def)?;
        let ty = todo!();
        (ty, TirExprKind::Def(value_def_id, vec![]))
      }
      ExprKind::Do(label, block) => todo!(),
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
      ExprKind::While(label, cond, block) => todo!(),
      ExprKind::Loop(label, block) => todo!(),
      ExprKind::Fn(flex, pats, ty, block) => todo!(),
      ExprKind::Return(value) => {
        if let Some(ty) = self.return_ty {
          let nil = self.types.nil();
          if let Some(value) = value {
            let value = self.resolve_expr_ty(value, ty);
            (self.types.new_var(), TirExprKind::Return(Some(Box::new(value))))
          } else if self.types.unify(ty, nil) {
            (self.types.new_var(), TirExprKind::Return(None))
          } else {
            Err(self.core.report(Diag::MissingReturnExpr { span, ty: self.types.show(ty) }))?
          }
        } else {
          Err(self.core.report(Diag::NoReturn { span }))?
        }
      }
      ExprKind::Break(label, expr) => todo!(),
      ExprKind::Continue(label) => todo!(),
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
        let ty = if self.types.unify(value.ty, space.ty) {
          value.ty
        } else {
          self.types.error(self.core.report(Diag::MismatchedValueSpaceTypes {
            span,
            value: self.types.show(value.ty),
            space: self.types.show(space.ty),
          }))
        };
        (ty, TirExprKind::Place(Box::new(value), Box::new(space)))
      }
      ExprKind::Tuple(els) => {
        let (tys, els) = els.iter().map(|e| self.resolve_expr(e)).map(|e| (e.ty, e)).collect();
        (self.types.new(TypeKind::Tuple(tys)), TirExprKind::Adt(None, els))
      }
      ExprKind::Object(entries) => {
        let (tys, els) = entries
          .iter()
          .map(|(k, e)| (k.ident, self.resolve_expr(e)))
          .map(|(k, e)| ((k, e.ty), e))
          .collect();
        (self.types.new(TypeKind::Object(tys)), TirExprKind::Adt(None, els))
      }
      ExprKind::List(els) => {
        let list = self.require_builtin(span, "List", |b| b.list)?;
        let el_ty = self.types.new_var();
        let els = els.iter().map(|e| self.resolve_expr_ty(e, el_ty)).collect();
        (self.types.new(TypeKind::Adt(list, vec![el_ty])), TirExprKind::List(els))
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
      ExprKind::Method(expr, ident, generics, exprs) => todo!(),
      ExprKind::Call(expr, exprs) => todo!(),
      ExprKind::Neg(expr) => todo!(),
      ExprKind::BinaryOp(binary_op, expr, expr1) => todo!(),
      ExprKind::Not(expr) => todo!(),
      ExprKind::Is(..) | ExprKind::LogicalOp(..) => {
        let bool = self.require_builtin_ty(span, "Bool", |b| b.bool)?;
        (bool, self._resolve_cond(bool, expr))
      }
      ExprKind::ComparisonOp(expr, items) => todo!(),
      ExprKind::BinaryOpAssign(binary_op, expr, expr1) => todo!(),
      ExprKind::Cast(expr, ty, _) => todo!(),
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
        let string = self.types.new(TypeKind::Adt(string, vec![]));
        let rest = rest
          .iter()
          .map(|(interp, seg)| {
            // todo
            let interp = self.resolve_expr_ty(interp, string);
            (interp, seg.content.clone())
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
    if !self.types.unify(ty, pat.ty) {
      self.core.report(Diag::ExpectedTypeFound {
        span: pat.span,
        expected: self.types.show(ty),
        found: self.types.show(pat.ty),
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
    Ok(match &pat.kind {
      PatKind::Hole => todo!(),
      PatKind::Paren(inner) => self._resolve_pat(inner)?,
      PatKind::Annotation(inner, ty) => {
        let ty = self.resolve_ty(ty);
        let inner = self.resolve_pat_ty(inner, ty);
        (ty, inner.kind)
      }
      PatKind::PathCall(path, pats) => todo!(),
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
        (self.types.new(TypeKind::Tuple(tys)), TirPatKind::Adt(None, els))
      }
      PatKind::Object(entries) => {
        // todo
        let (tys, els) = entries
          .iter()
          .map(|(k, e)| (k.ident, self.resolve_pat(e)))
          .map(|(k, e)| ((k, e.ty), e))
          .collect();
        (self.types.new(TypeKind::Object(tys)), TirPatKind::Adt(None, els))
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
    todo!()
  }

  fn push_scope(&mut self) {}
  fn pop_scope(&mut self) {}

  fn resolve_block(&self, block: &Block<'core>) -> TirBlock<'core> {
    todo!()
  }

  fn resolve_block_ty(&self, block: &Block<'core>, result: Type) -> TirBlock<'core> {
    todo!()
  }

  fn resolve_tuple_field(
    &mut self,
    span: Span,
    ty: Type,
    index: usize,
  ) -> Result<(usize, Type), ErrorGuaranteed> {
    match self.types.kind(ty) {
      TypeKind::Tuple(els) => {
        if index < els.len() {
          return Ok((els.len(), els[index]));
        }
      }
      TypeKind::Adt(adt_id, type_params) => todo!(),
      TypeKind::Inverse(ty) => {
        let (len, ty) = self.resolve_tuple_field(span, *ty, index)?;
        return Ok((len, self.types.inverse(ty)));
      }
      TypeKind::Error(err) => return Err(*err),
      _ => {}
    }
    Err(self.core.report(Diag::MissingTupleField { span, ty: self.types.show(ty), index }))
  }

  fn resolve_object_field(
    &mut self,
    span: Span,
    ty: Type,
    key: Ident<'core>,
  ) -> Result<(usize, usize, Type), ErrorGuaranteed> {
    match self.types.kind(ty) {
      TypeKind::Object(entries) => {
        if let Some((index, (_, &ty))) = entries.iter().enumerate().find(|&(_, (&k, _))| k == key) {
          return Ok((index, entries.len(), ty));
        }
      }
      TypeKind::Adt(adt_id, type_params) => todo!(),
      TypeKind::Inverse(ty) => {
        let (index, len, ty) = self.resolve_object_field(span, *ty, key)?;
        return Ok((index, len, self.types.inverse(ty)));
      }
      TypeKind::Error(err) => return Err(*err),
      _ => {}
    }
    Err(self.core.report(Diag::MissingObjectField { span, ty: self.types.show(ty), key }))
  }
}
