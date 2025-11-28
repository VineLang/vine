use crate::{
  components::{distiller::Distiller, matcher::Row, resolver::Resolver},
  structures::{
    ast::{Expr, ExprKind, LogicalOp, Pat, Span},
    chart::FnId,
    diag::Diag,
    resolutions::FnRel,
    tir::{TirExpr, TirExprKind, TirImpl},
    types::Type,
    vir::{Interface, InterfaceKind, Layer, Port, PortKind, Stage, Transfer},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_bool(&self, bool: bool) -> Doc<'src> {
    Doc(if bool { "true" } else { "false" })
  }

  pub(crate) fn fmt_expr_not(&self, expr: &Expr) -> Doc<'src> {
    Doc::concat([Doc("!"), self.fmt_expr(expr)])
  }

  pub(crate) fn fmt_expr_is(&self, expr: &Expr, pat: &Pat) -> Doc<'src> {
    Doc::concat([self.fmt_expr(expr), Doc(" is "), self.fmt_pat(pat)])
  }

  pub(crate) fn fmt_expr_logical_op(&self, op: &LogicalOp, lhs: &Expr, rhs: &Expr) -> Doc<'src> {
    Doc::concat([
      self.fmt_expr(lhs),
      Doc(match op {
        LogicalOp::And => " and ",
        LogicalOp::Or => " or ",
        LogicalOp::Implies => " impl ",
      }),
      self.fmt_expr(rhs),
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_not(&mut self, span: Span, inner: &Expr) -> Result<TirExpr, Diag> {
    let inner = self.resolve_expr(inner);
    let return_ty = self.types.new_var(span);
    let rel = self.builtin_fn(span, self.chart.builtins.not, "not", [inner.ty, return_ty])?;
    if let FnRel::Item(FnId::Abstract(..), impls) = &self.rels.fns[rel]
      && let [TirImpl::Def(id, _)] = **impls
      && self.chart.builtins.bool_not == Some(id)
    {
      return Ok(TirExpr::new(span, return_ty, TirExprKind::Not(inner)));
    }
    Ok(TirExpr::new(span, return_ty, TirExprKind::Call(rel, None, vec![inner])))
  }

  pub(crate) fn resolve_cond(&mut self, cond: &Expr) -> TirExpr {
    self.enter_scope();
    let result = self.resolve_scoped_cond(cond);
    self.exit_scope();
    result
  }

  pub(crate) fn resolve_scoped_cond(&mut self, cond: &Expr) -> TirExpr {
    match self._resolve_scoped_cond(cond) {
      Ok(kind) => {
        let ty = self.bool(cond.span);
        TirExpr { span: cond.span, ty, kind: Box::new(kind) }
      }
      Err(diag) => self.error_expr(cond.span, diag),
    }
  }

  pub(crate) fn _resolve_scoped_cond(&mut self, cond: &Expr) -> Result<TirExprKind, Diag> {
    let span = cond.span;
    Ok(match &*cond.kind {
      ExprKind::Error(err) => Err(*err)?,
      ExprKind::Bool(bool) => TirExprKind::Bool(*bool),
      ExprKind::Is(expr, pat) => {
        let expr = self.resolve_expr(expr);
        let pat = self.resolve_pat_type(pat, expr.ty);
        TirExprKind::Is(expr, pat)
      }
      ExprKind::LogicalOp(LogicalOp::And, a, b) => {
        let a = self.resolve_scoped_cond(a);
        let b = self.resolve_scoped_cond(b);
        TirExprKind::LogicalOp(LogicalOp::And, a, b)
      }
      ExprKind::LogicalOp(LogicalOp::Or, a, b) => {
        self.enter_scope();
        let a = self.resolve_scoped_cond(a);
        self.exit_scope();
        self.enter_scope();
        let b = self.resolve_scoped_cond(b);
        self.exit_scope();
        TirExprKind::LogicalOp(LogicalOp::Or, a, b)
      }
      ExprKind::LogicalOp(LogicalOp::Implies, a, b) => {
        self.enter_scope();
        let a = self.resolve_scoped_cond(a);
        let b = self.resolve_scoped_cond(b);
        self.exit_scope();
        TirExprKind::LogicalOp(LogicalOp::Implies, a, b)
      }
      _ => {
        let bool = self.bool(span);
        *self.resolve_expr_type(cond, bool).kind
      }
    })
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_cond_bool(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    cond: &TirExpr,
    negate: bool,
  ) -> Port {
    match &*cond.kind {
      TirExprKind![!cond] => {
        let value = self.distill_expr_value(stage, cond);
        if negate {
          stage.ext_fn(span, "n32_eq", false, value, Port { ty, kind: PortKind::N32(0) }, ty)
        } else {
          value
        }
      }
      TirExprKind::Bool(bool) => Port { ty, kind: PortKind::N32((*bool ^ negate) as u32) },
      TirExprKind::Not(inner) => self.distill_cond_bool(stage, span, ty, inner, !negate),
      _ => {
        let local = self.new_local(stage, span, cond.ty);
        let (mut sub_layer, mut sub_stage) = self.child_layer(stage, span);
        let (mut true_stage, mut false_stage) =
          self.distill_cond(&mut sub_layer, &mut sub_stage, span, cond);
        true_stage.local_barrier_write_to(local, Port { ty, kind: PortKind::N32(!negate as u32) });
        false_stage.local_barrier_write_to(local, Port { ty, kind: PortKind::N32(negate as u32) });
        self.finish_stage(true_stage);
        self.finish_stage(false_stage);
        self.finish_stage(sub_stage);
        self.finish_layer(sub_layer);
        stage.local_read_barrier(local, span, ty)
      }
    }
  }

  pub(crate) fn distill_cond(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    span: Span,
    cond: &TirExpr,
  ) -> (Stage, Stage) {
    match &*cond.kind {
      TirExprKind![!cond] => {
        let bool = self.distill_expr_value(stage, cond);
        let interface = self.interfaces.push(None);
        let true_stage = self.new_stage(layer, span, interface);
        let false_stage = self.new_stage(layer, span, interface);
        self.interfaces[interface] = Some(Interface::new(
          interface,
          layer.id,
          InterfaceKind::Branch(false_stage.id, true_stage.id),
        ));
        stage.transfer = Some(Transfer { interface, data: Some(bool) });
        (true_stage, false_stage)
      }
      TirExprKind::Bool(bool) => {
        let true_stage = self.new_unconditional_stage(layer, span);
        let false_stage = self.new_unconditional_stage(layer, span);
        stage.transfer = Some(Transfer::unconditional(if *bool {
          true_stage.interface
        } else {
          false_stage.interface
        }));
        (true_stage, false_stage)
      }
      TirExprKind::Not(inner) => swap(self.distill_cond(layer, stage, span, inner)),
      TirExprKind::Is(value, pat) => {
        let value = self.distill_expr_value(stage, value);
        let true_stage = self.new_unconditional_stage(layer, span);
        let false_stage = self.new_unconditional_stage(layer, span);
        self.distill_pattern_match(
          span,
          layer,
          stage,
          value,
          vec![Row::new(Some(pat), true_stage.interface), Row::new(None, false_stage.interface)],
        );
        (true_stage, false_stage)
      }
      TirExprKind::LogicalOp(op, a, b) => {
        let (invert_out, invert_a, invert_b) = match op {
          LogicalOp::And => (false, false, false),
          LogicalOp::Or => (true, true, true), // A || B == !(!A && !B)
          LogicalOp::Implies => (true, false, true), // A => B == !(A && !B)
        };
        let (mut a_true, mut a_false) = swap_if(invert_a, self.distill_cond(layer, stage, span, a));
        let (b_true, mut b_false) =
          swap_if(invert_b, self.distill_cond(layer, &mut a_true, span, b));
        let true_stage = b_true;
        let false_stage = self.new_unconditional_stage(layer, span);
        a_false.transfer = Some(Transfer::unconditional(false_stage.interface));
        b_false.transfer = Some(Transfer::unconditional(false_stage.interface));
        self.finish_stage(a_true);
        self.finish_stage(a_false);
        self.finish_stage(b_false);
        swap_if(invert_out, (true_stage, false_stage))
      }
    }
  }
}

fn swap<T>((a, b): (T, T)) -> (T, T) {
  (b, a)
}

fn swap_if<T>(bool: bool, (a, b): (T, T)) -> (T, T) {
  if bool { (b, a) } else { (a, b) }
}
