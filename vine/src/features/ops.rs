use crate::{
  components::{distiller::Distiller, resolver::Resolver},
  structures::{
    ast::{BinaryOp, ComparisonOp, Expr, Sign, Span},
    diag::Diag,
    resolutions::FnRelId,
    tir::{TirExpr, TirExprKind},
    types::{Type, TypeKind},
    vir::{Port, PortKind, Stage, Step},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_assign(
    &self,
    inverted: bool,
    space: &Expr<'core>,
    value: &Expr<'core>,
  ) -> Doc<'src> {
    Doc::concat([
      self.fmt_expr(space),
      Doc(if inverted { " ~= " } else { " = " }),
      self.fmt_expr(value),
    ])
  }

  pub(crate) fn fmt_expr_sign(&self, sign: Sign, expr: &Expr<'core>) -> Doc<'src> {
    Doc::concat([
      Doc(match sign {
        Sign::Pos => "+",
        Sign::Neg => "-",
      }),
      self.fmt_expr(expr),
    ])
  }

  pub(crate) fn fmt_expr_binary_op(
    &self,
    op: &BinaryOp,
    lhs: &Expr<'core>,
    rhs: &Expr<'core>,
  ) -> Doc<'src> {
    Doc::concat([self.fmt_expr(lhs), Doc(" "), Doc(op.as_str()), Doc(" "), self.fmt_expr(rhs)])
  }

  pub(crate) fn fmt_expr_binary_op_assign(
    &self,
    op: &BinaryOp,
    lhs: &Expr<'core>,
    rhs: &Expr<'core>,
  ) -> Doc<'src> {
    Doc::concat([self.fmt_expr(lhs), Doc(" "), Doc(op.as_str()), Doc("= "), self.fmt_expr(rhs)])
  }

  pub(crate) fn fmt_expr_comparison_op(
    &self,
    init: &Expr<'core>,
    cmps: &Vec<(ComparisonOp, Expr<'core>)>,
  ) -> Doc<'src> {
    Doc::concat([self.fmt_expr(init)].into_iter().chain(
      cmps.iter().flat_map(|(op, x)| [Doc(" "), Doc(op.as_str()), Doc(" "), self.fmt_expr(x)]),
    ))
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_assign(
    &mut self,
    span: Span,
    dir: bool,
    space: &Expr<'core>,
    value: &Expr<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let space = self.resolve_expr(space);
    let value = self.resolve_expr_type(value, space.ty);
    Ok(TirExpr::new(span, self.types.nil(), TirExprKind::Assign(dir, space, value)))
  }

  pub(crate) fn resolve_expr_sign(
    &mut self,
    span: Span,
    sign: Sign,
    inner: &Expr<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let inner = self.resolve_expr(inner);
    let return_ty = self.types.new_var(span);
    let rel = match sign {
      Sign::Pos => self.builtin_fn(span, self.chart.builtins.pos, "pos", [inner.ty, return_ty])?,
      Sign::Neg => self.builtin_fn(span, self.chart.builtins.neg, "neg", [inner.ty, return_ty])?,
    };
    Ok(TirExpr::new(span, return_ty, TirExprKind::Call(rel, None, vec![inner])))
  }

  pub(crate) fn resolve_expr_binary_op(
    &mut self,
    span: Span,
    op: BinaryOp,
    lhs: &Expr<'core>,
    rhs: &Expr<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let lhs = self.resolve_expr(lhs);
    let rhs = self.resolve_expr(rhs);
    let return_ty = self.types.new_var(span);
    let fn_id = self.chart.builtins.binary_ops.get(&op).copied().flatten();
    let rel = self.builtin_fn(span, fn_id, op.as_str(), [lhs.ty, rhs.ty, return_ty])?;
    Ok(TirExpr::new(span, return_ty, TirExprKind::Call(rel, None, vec![lhs, rhs])))
  }

  pub(crate) fn resolve_expr_binary_op_assign(
    &mut self,
    span: Span,
    op: BinaryOp,
    lhs: &Expr<'core>,
    rhs: &Expr<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let lhs = self.resolve_expr(lhs);
    let rhs = self.resolve_expr(rhs);
    let fn_id = self.chart.builtins.binary_ops.get(&op).copied().flatten();
    let rel = self.builtin_fn(span, fn_id, op.as_str(), [lhs.ty, rhs.ty, lhs.ty])?;
    Ok(TirExpr::new(span, self.types.nil(), TirExprKind::CallAssign(rel, lhs, rhs)))
  }

  pub(crate) fn resolve_expr_comparison_op(
    &mut self,
    span: Span,
    init: &Expr<'core>,
    cmps: &Vec<(ComparisonOp, Expr<'core>)>,
  ) -> Result<TirExpr, Diag<'core>> {
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
    Ok(TirExpr::new(span, self.bool(span), TirExprKind::CallCompare(init, cmps)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_expr_nil_assign(
    &mut self,
    stage: &mut Stage,
    inverse: bool,
    space: &TirExpr,
    value: &TirExpr,
  ) {
    if inverse {
      let space = self.distill_expr_space(stage, space);
      let value = self.distill_expr_value(stage, value);
      stage.steps.push(Step::Link(space, value));
    } else {
      let value = self.distill_expr_value(stage, value);
      let space = self.distill_expr_space(stage, space);
      stage.steps.push(Step::Link(space, value));
    }
  }

  pub(crate) fn distill_expr_nil_call_assign(
    &mut self,
    stage: &mut Stage,
    span: Span,
    func: FnRelId,
    lhs: &TirExpr,
    rhs: &TirExpr,
  ) {
    let rhs = self.distill_expr_value(stage, rhs);
    let (lhs, out) = self.distill_expr_place(stage, lhs);
    stage.steps.push(Step::Call(span, func, None, vec![lhs, rhs], out));
  }

  pub(crate) fn distill_expr_value_call_compare(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    init: &TirExpr,
    cmps: &[(FnRelId, TirExpr)],
  ) -> Port {
    let mut last_result = Port { ty, kind: PortKind::Nil };
    let lhs = self.distill_expr_place(stage, init);
    let ref_ty = self.types.new(TypeKind::Ref(init.ty));
    let mut lhs = Some(stage.ref_place(span, ref_ty, lhs));
    for (i, (func, rhs)) in cmps.iter().enumerate() {
      let first = i == 0;
      let last = i == cmps.len() - 1;
      let rhs = self.distill_expr_place(stage, rhs);
      let (rhs, next_lhs) = if last {
        (stage.ref_place(span, ref_ty, rhs), None)
      } else {
        let wire = stage.new_wire(span, init.ty);
        (
          stage.ref_place(span, ref_ty, (rhs.0, wire.neg)),
          Some(stage.ref_place(span, ref_ty, (wire.pos, rhs.1))),
        )
      };
      let result = stage.new_wire(span, ty);
      stage.steps.push(Step::Call(span, *func, None, vec![lhs.unwrap(), rhs], result.neg));
      last_result = if first {
        result.pos
      } else {
        stage.ext_fn(span, "n32_and", false, last_result, result.pos, ty)
      };
      lhs = next_lhs;
    }
    last_result
  }
}
