use crate::structures::{
  ast::{LogicalOp, Span},
  chart::VariantId,
  tir::{ClosureId, LabelId, TirClosure, TirExpr, TirExprKind, TirLocal, TirPat, TirPatKind},
  types::Type,
  vir::{
    Header, Interface, InterfaceId, InterfaceKind, Layer, Port, PortKind, Stage, Step, Transfer,
  },
};

use super::{pattern_matching::Row, Distiller, Label, Return};

impl<'core, 'r> Distiller<'core, 'r> {
  pub(super) fn distill_closure_def(&mut self, closure: &TirClosure) -> InterfaceId {
    let span = closure.span;
    let mut layer = self.new_layer();
    let interface = self.interfaces.push(None);

    let call = {
      let mut stage = self.new_stage(&mut layer, span, interface);

      let return_local = self.new_local(&mut stage, span, closure.body.ty);
      let params =
        closure.params.iter().map(|p| self.distill_pat_value(&mut stage, p)).collect::<Vec<_>>();
      let result = stage.local_read_barrier(return_local, span, closure.body.ty);
      stage.header = Header::Fn(params, result);

      self.returns.push(Return { ty: closure.body.ty, layer: stage.layer, local: return_local });
      let result = self.distill_expr_value(&mut stage, &closure.body);
      stage.local_barrier_write_to(return_local, result);
      self.returns.pop();

      self.finish_stage(stage)
    };

    let fork = closure.flex.fork().then(|| {
      let mut stage = self.new_stage(&mut layer, span, interface);
      let former = stage.new_wire(span, closure.ty);
      let latter = stage.new_wire(span, closure.ty);
      stage.steps.push(Step::Transfer(Transfer { interface, data: Some(former.neg) }));
      stage.steps.push(Step::Transfer(Transfer { interface, data: Some(latter.neg) }));
      stage.header = Header::Fork(former.pos, latter.pos);
      self.finish_stage(stage)
    });

    let drop = closure.flex.drop().then(|| {
      let mut stage = self.new_stage(&mut layer, span, interface);
      stage.header = Header::Drop;
      self.finish_stage(stage)
    });

    self.interfaces[interface] =
      Some(Interface::new(interface, layer.id, InterfaceKind::Fn { call, fork, drop }));

    self.finish_layer(layer);
    interface
  }

  pub(super) fn distill_closure(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    closure_id: ClosureId,
  ) -> Port {
    let interface = self.closures[closure_id];
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Transfer(Transfer { interface, data: Some(wire.neg) }));
    wire.pos
  }

  pub(super) fn distill_seq(
    &mut self,
    stage: &mut Stage,
    ignored: &TirExpr,
    continuation: &TirExpr,
  ) -> Port {
    self.distill_expr_nil(stage, ignored);
    self.distill_expr_value(stage, continuation)
  }

  pub(super) fn distill_let(
    &mut self,
    stage: &mut Stage,
    pat: &TirPat,
    init: &Option<TirExpr>,
    continuation: &TirExpr,
  ) -> Port {
    if let Some(init) = init {
      let value = self.distill_expr_value(stage, init);
      let pat = self.distill_pat_value(stage, pat);
      stage.steps.push(Step::Link(pat, value));
    } else {
      self.distill_pat_nil(stage, pat);
    }
    self.distill_expr_value(stage, continuation)
  }

  pub(super) fn distill_let_else(
    &mut self,
    stage: &mut Stage,
    span: Span,
    pat: &TirPat,
    init: &TirExpr,
    else_block: &TirExpr,
    continuation: &TirExpr,
  ) -> Port {
    let local = self.new_local(stage, span, continuation.ty);
    stage.local_barrier(local);
    let (mut layer, mut init_stage) = self.child_layer(stage, span);
    let value = self.distill_expr_value(&mut init_stage, init);
    let mut then_stage = self.new_unconditional_stage(&mut layer, span);
    let mut else_stage = self.new_unconditional_stage(&mut layer, span);
    self.distill_pattern_match(
      span,
      &mut layer,
      &mut init_stage,
      value,
      vec![Row::new(Some(pat), then_stage.interface), Row::new(None, else_stage.interface)],
    );
    let result = self.distill_expr_value(&mut then_stage, continuation);
    then_stage.local_barrier_write_to(local, result);
    let result = self.distill_expr_value(&mut else_stage, else_block);
    let never = result.ty;
    else_stage.steps.push(Step::Link(result, Port { ty: never.inverse(), kind: PortKind::Nil }));
    self.finish_stage(init_stage);
    self.finish_stage(then_stage);
    self.finish_stage(else_stage);
    self.finish_layer(layer);
    stage.local_read_barrier(local, span, continuation.ty)
  }

  pub(super) fn distill_match(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    value: &TirExpr,
    arms: &[(TirPat, TirExpr)],
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (mut layer, mut init_stage) = self.child_layer(stage, span);
    let value = self.distill_expr_value(&mut init_stage, value);
    let rows = arms
      .iter()
      .map(|(pat, expr)| {
        let mut stage = self.new_unconditional_stage(&mut layer, span);
        let result = self.distill_expr_value(&mut stage, expr);
        stage.local_barrier_write_to(local, result);
        let interface = stage.interface;
        self.finish_stage(stage);
        Row::new(Some(pat), interface)
      })
      .collect();
    self.distill_pattern_match(span, &mut layer, &mut init_stage, value, rows);
    self.finish_stage(init_stage);
    self.finish_layer(layer);
    stage.local_read_barrier(local, span, ty)
  }

  pub(super) fn distill_continue(&mut self, stage: &mut Stage, label: LabelId) {
    let label = self.labels[label].as_ref().unwrap();
    let transfer = Transfer::unconditional(label.continue_transfer.unwrap());
    stage.steps.push(Step::Diverge(label.layer, Some(transfer)));
  }

  pub(super) fn distill_break(
    &mut self,
    stage: &mut Stage,
    label: LabelId,
    value: &Option<TirExpr>,
  ) {
    let value = match value {
      Some(v) => self.distill_expr_value(stage, v),
      None => Port { ty: self.types.nil(), kind: PortKind::Nil },
    };

    let label = self.labels[label].as_ref().unwrap();
    if let Some(local) = label.break_value {
      stage.local_barrier_write_to(local, value);
    } else {
      stage.steps.push(Step::Link(value, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    }

    stage.steps.push(Step::Diverge(label.layer, None));
  }

  pub(super) fn distill_return(&mut self, stage: &mut Stage, value: &Option<TirExpr>) {
    let value = match value {
      Some(v) => self.distill_expr_value(stage, v),
      None => Port { ty: self.types.nil(), kind: PortKind::Nil },
    };
    let return_ = self.returns.last().unwrap();

    stage.local_barrier_write_to(return_.local, value);
    stage.steps.push(Step::Diverge(return_.layer, None));
  }

  pub(super) fn distill_loop(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    label: LabelId,
    block: &TirExpr,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (layer, mut body_stage) = self.child_layer(stage, span);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(body_stage.interface),
      break_value: Some(local),
    });

    let result = self.distill_expr_value(&mut body_stage, block);
    body_stage.steps.push(Step::Link(result, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    body_stage.transfer = Some(Transfer::unconditional(body_stage.interface));

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }

  pub(super) fn distill_while(
    &mut self,
    stage: &mut Stage,
    span: Span,
    label: LabelId,
    cond: &TirExpr,
    block: &TirExpr,
  ) {
    let (mut layer, mut cond_stage) = self.child_layer(stage, span);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(cond_stage.interface),
      break_value: None,
    });

    let (mut then_stage, else_stage) = self.distill_cond(&mut layer, &mut cond_stage, span, cond);

    let result = self.distill_expr_value(&mut then_stage, block);
    then_stage.steps.push(Step::Link(result, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    then_stage.transfer = Some(Transfer::unconditional(cond_stage.interface));

    self.finish_stage(cond_stage);
    self.finish_stage(then_stage);
    self.finish_stage(else_stage);
    self.finish_layer(layer);
  }

  pub(super) fn distill_if(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    arms: &[(TirExpr, TirExpr)],
    leg: &Option<TirExpr>,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (mut layer, mut cur_stage) = self.child_layer(stage, span);

    for (cond, block) in arms {
      let (mut then_stage, else_stage) = self.distill_cond(&mut layer, &mut cur_stage, span, cond);
      let result = self.distill_expr_value(&mut then_stage, block);
      then_stage.local_barrier_write_to(local, result);
      self.finish_stage(cur_stage);
      self.finish_stage(then_stage);
      cur_stage = else_stage;
    }

    if let Some(leg) = leg {
      let result = self.distill_expr_value(&mut cur_stage, leg);
      cur_stage.local_barrier_write_to(local, result);
    } else {
      cur_stage.local_barrier_write_to(local, Port { ty: self.types.nil(), kind: PortKind::Nil });
    }

    self.finish_stage(cur_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }

  pub(super) fn distill_do(
    &mut self,
    stage: &mut Stage,
    span: Span,
    label: LabelId,
    block: &TirExpr,
  ) -> Port {
    let local = self.new_local(stage, span, block.ty);
    let (layer, mut body_stage) = self.child_layer(stage, span);

    *self.labels.get_or_extend(label) =
      Some(Label { layer: layer.id, continue_transfer: None, break_value: Some(local) });

    let result = self.distill_expr_value(&mut body_stage, block);
    body_stage.local_barrier_write_to(local, result);

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, block.ty)
  }

  pub(super) fn distill_cond_bool(
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

  pub(super) fn distill_cond(
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

  pub(super) fn distill_try(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    ok_ty: Type,
    err_ty: Type,
    result: &TirExpr,
  ) -> Port {
    let result_id = self.chart.builtins.result.unwrap();
    let ok_variant = VariantId(0);
    let err_variant = VariantId(1);

    let (mut layer, mut init_stage) = self.child_layer(stage, span);
    let result = self.distill_expr_value(&mut init_stage, result);
    let mut ok_stage = self.new_unconditional_stage(&mut layer, span);
    let mut err_stage = self.new_unconditional_stage(&mut layer, span);
    let ok_local = self.locals.push(TirLocal { span, ty: ok_ty });
    let err_local = self.locals.push(TirLocal { span, ty: err_ty });
    let pat = |variant_id, content_ty, local| TirPat {
      span,
      ty,
      kind: Box::new(TirPatKind::Enum(
        result_id,
        variant_id,
        Some(TirPat { span, ty: content_ty, kind: Box::new(TirPatKind::Local(local)) }),
      )),
    };

    self.distill_pattern_match(
      span,
      &mut layer,
      &mut init_stage,
      result,
      vec![
        Row::new(Some(&pat(ok_variant, ok_ty, ok_local)), ok_stage.interface),
        Row::new(Some(&pat(err_variant, err_ty, err_local)), err_stage.interface),
      ],
    );
    self.finish_stage(init_stage);

    let out_local = self.new_local(stage, span, ty);
    let value = ok_stage.local_read_barrier(ok_local, span, ok_ty);
    ok_stage.local_barrier_write_to(out_local, value);
    self.finish_stage(ok_stage);

    let ret = self.returns.last().unwrap();
    let err = err_stage.local_read_barrier(err_local, span, err_ty);
    let result = err_stage.new_wire(span, ret.ty);
    err_stage.steps.push(Step::Enum(result_id, err_variant, result.neg, Some(err)));
    err_stage.local_barrier_write_to(ret.local, result.pos);
    err_stage.steps.push(Step::Diverge(ret.layer, None));
    self.finish_stage(err_stage);

    self.finish_layer(layer);
    stage.local_read_barrier(out_local, span, ty)
  }
}

fn swap<T>((a, b): (T, T)) -> (T, T) {
  (b, a)
}

fn swap_if<T>(bool: bool, (a, b): (T, T)) -> (T, T) {
  if bool {
    (b, a)
  } else {
    (a, b)
  }
}
