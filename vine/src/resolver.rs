use crate::{
  ast::{Expr, ExprKind, Span, Ty},
  chart::{Builtins, Chart},
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  tir::{TirExpr, TirExprKind},
  types::{Type, TypeKind, Types},
};

mod resolve_path;

#[derive(Debug)]
pub struct Resolver<'core, 'ctx> {
  pub core: &'core Core<'core>,
  pub chart: &'ctx mut Chart<'core>,
  pub types: Types<'core>,
}

impl<'core> Resolver<'core, '_> {
  pub fn resolve_expr_ty(&mut self, expr: &Expr<'core>, ty: Type) -> TirExpr<'core> {
    todo!()
  }

  pub fn resolve_expr(&mut self, expr: &Expr<'core>) -> TirExpr<'core> {
    let (ty, kind) = self._resolve_expr(expr).unwrap_or_else(|diag| {
      let err = self.core.report(diag);
      (self.types.error(err), TirExprKind::Error(err))
    });
    TirExpr { span: expr.span, ty, kind }
  }

  pub fn _resolve_expr(
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
      ExprKind::Match(expr, items) => todo!(),
      ExprKind::If(then_branches, else_branch) => todo!(),
      ExprKind::While(label, expr, block) => todo!(),
      ExprKind::Loop(label, block) => todo!(),
      ExprKind::Fn(flex, pats, ty, block) => todo!(),
      ExprKind::Return(expr) => todo!(),
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
      ExprKind::TupleField(expr, _) => todo!(),
      ExprKind::ObjectField(expr, key) => todo!(),
      ExprKind::Method(expr, ident, generics, exprs) => todo!(),
      ExprKind::Call(expr, exprs) => todo!(),
      ExprKind::Neg(expr) => todo!(),
      ExprKind::BinaryOp(binary_op, expr, expr1) => todo!(),
      ExprKind::Not(expr) => todo!(),
      ExprKind::Is(expr, pat) => todo!(),
      ExprKind::LogicalOp(logical_op, expr, expr1) => todo!(),
      ExprKind::ComparisonOp(expr, items) => todo!(),
      ExprKind::BinaryOpAssign(binary_op, expr, expr1) => todo!(),
      ExprKind::Cast(expr, ty, _) => todo!(),
      ExprKind::Bool(value) => {
        let bool = self.require_builtin(span, "Bool", |b| b.bool)?;
        (self.types.new(TypeKind::Opaque(bool)), TirExprKind::Bool(*value))
      }
      ExprKind::N32(value) => {
        let n32 = self.require_builtin(span, "N32", |b| b.n32)?;
        (self.types.new(TypeKind::Opaque(n32)), TirExprKind::N32(*value))
      }
      ExprKind::F32(value) => {
        let f32 = self.require_builtin(span, "F32", |b| b.f32)?;
        (self.types.new(TypeKind::Opaque(f32)), TirExprKind::F32(*value))
      }
      ExprKind::Char(value) => {
        let char = self.require_builtin(span, "Char", |b| b.char)?;
        (self.types.new(TypeKind::Opaque(char)), TirExprKind::Char(*value))
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

  fn resolve_ty(&mut self, ty: &Ty<'core>) -> Type {
    todo!()
  }
}
