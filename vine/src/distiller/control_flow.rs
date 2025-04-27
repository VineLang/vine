use crate::{
  ast::LogicalOp,
  tir::{LabelId, Local, TirBlock, TirBlockKind, TirClosure, TirExpr, TirExprKind, TirPat},
  vir::{Header, Interface, InterfaceId, InterfaceKind, Layer, Port, Stage, Step, Transfer},
};

use super::{pattern_matching::Row, Distiller, Label, Return};

impl<'core, 'r> Distiller<'core, 'r> {
  pub fn distill_block(&mut self, stage: &mut Stage, block: &TirBlock<'core>) -> Port {
    let ty = block.ty;
    match &block.kind {
      TirBlockKind::Nil => Port::Erase,
      TirBlockKind::Error(e) => Port::Error(*e),
      TirBlockKind::Let(pat, expr, block) => {
        if let Some(expr) = expr {
          let expr = self.distill_expr_value(stage, expr);
          let pat = self.distill_pat_value(stage, pat);
          stage.steps.push(Step::Link(pat, expr));
        } else {
          self.distill_pat_nil(stage, pat);
        }
        self.distill_block(stage, block)
      }
      TirBlockKind::LetElse(pat, expr, else_block, block) => {
        let local = self.new_local(stage);
        stage.erase_local(local);
        let (mut layer, mut init_stage) = self.child_layer(stage);
        let value = self.distill_expr_value(&mut init_stage, expr);
        let mut then_stage = self.new_unconditional_stage(&mut layer);
        let mut else_stage = self.new_unconditional_stage(&mut layer);
        self.distill_pattern_match(
          &mut layer,
          &mut init_stage,
          value,
          vec![Row::new(Some(pat), then_stage.interface), Row::new(None, else_stage.interface)],
        );
        let result = self.distill_block(&mut then_stage, block);
        then_stage.set_local_to(local, result);
        let result = self.distill_block(&mut else_stage, else_block);
        self.drop(expr.span, &mut else_stage, ty, result);
        self.finish_stage(init_stage);
        self.finish_stage(then_stage);
        self.finish_stage(else_stage);
        self.finish_layer(layer);
        stage.take_local(local)
      }
      TirBlockKind::Seq(expr, block) => {
        let value = self.distill_expr_value(stage, expr);
        self.drop(expr.span, stage, ty, value);
        self.distill_block(stage, block)
      }
      TirBlockKind::Expr(expr) => self.distill_expr_value(stage, expr),
    }
  }

  pub(super) fn distill_match(
    &mut self,
    stage: &mut Stage,
    value: &TirExpr<'core>,
    arms: &Vec<(TirPat, TirBlock<'core>)>,
  ) -> Port {
    let local = self.new_local(stage);
    let (mut layer, mut init_stage) = self.child_layer(stage);
    let value = self.distill_expr_value(&mut init_stage, value);
    let rows = arms
      .iter()
      .map(|(pat, expr)| {
        let mut stage = self.new_unconditional_stage(&mut layer);
        let result = self.distill_block(&mut stage, expr);
        stage.set_local_to(local, result);
        let interface = stage.interface;
        self.finish_stage(stage);
        Row::new(Some(pat), interface)
      })
      .collect();
    self.distill_pattern_match(&mut layer, &mut init_stage, value, rows);
    self.finish_stage(init_stage);
    self.finish_layer(layer);
    stage.take_local(local)
  }

  pub(super) fn distill_closure(&mut self, closure: &TirClosure<'core>) -> InterfaceId {
    let mut layer = self.new_layer();
    let interface = self.interfaces.push(None);
    let call = {
      let mut stage = self.new_stage(&mut layer, interface);

      let params = closure.params.iter().map(|p| self.distill_pat_value(&mut stage, p)).collect();
      let return_local = self.new_local(&mut stage);
      let result = stage.take_local(return_local);
      stage.header = Header::Fn(params, result);

      self.returns.push(Return { layer: layer.id, local: return_local });
      let result = self.distill_block(&mut stage, &closure.body);
      stage.set_local_to(return_local, result);
      self.returns.pop();

      self.finish_stage(stage)
    };
    let fork = closure.flex.fork().then(|| {
      let mut stage = self.new_stage(&mut layer, interface);
      let former = stage.new_wire();
      let latter = stage.new_wire();
      stage.steps.push(Step::Transfer(Transfer { interface, data: Some(former.0) }));
      stage.steps.push(Step::Transfer(Transfer { interface, data: Some(latter.0) }));
      stage.header = Header::Fork(former.1, latter.1);
      self.finish_stage(stage)
    });
    let drop = closure.flex.drop().then(|| {
      let mut stage = self.new_stage(&mut layer, interface);
      stage.header = Header::Drop;
      self.finish_stage(stage)
    });
    self.interfaces[interface] =
      Some(Interface::new(interface, layer.id, InterfaceKind::Closure { call, fork, drop }));
    self.finish_layer(layer);
    interface
  }

  pub(super) fn distill_continue(&mut self, stage: &mut Stage, label: LabelId) -> Port {
    let label = self.labels[label].as_ref().unwrap();
    let transfer = Transfer::unconditional(label.continue_transfer.unwrap());
    stage.steps.push(Step::Diverge(label.layer, Some(transfer)));
    Port::Erase
  }

  pub(super) fn distill_break(
    &mut self,
    stage: &mut Stage,
    label: LabelId,
    value: &Option<Box<TirExpr<'core>>>,
  ) -> Port {
    let value = value.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);

    let label = self.labels[label].as_ref().unwrap();
    if let Some(local) = label.break_value {
      stage.set_local_to(local, value);
    } else {
      assert!(matches!(value, Port::Erase));
    }

    stage.steps.push(Step::Diverge(label.layer, None));

    Port::Erase
  }

  pub(super) fn distill_return(
    &mut self,
    stage: &mut Stage,
    value: &Option<Box<TirExpr<'core>>>,
  ) -> Port {
    let value = value.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);
    let return_ = self.returns.last().unwrap();

    stage.set_local_to(return_.local, value);
    stage.steps.push(Step::Diverge(return_.layer, None));

    Port::Erase
  }

  pub(super) fn distill_loop(
    &mut self,
    stage: &mut Stage,
    label: LabelId,
    block: &TirBlock<'core>,
  ) -> Port {
    let local = self.new_local(stage);
    let (layer, mut body_stage) = self.child_layer(stage);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(body_stage.interface),
      break_value: Some(local),
    });

    let result = self.distill_block(&mut body_stage, block);
    self.drop(block.span, &mut body_stage, block.ty, result);
    body_stage.transfer = Some(Transfer::unconditional(body_stage.interface));

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.take_local(local)
  }

  pub(super) fn distill_while(
    &mut self,
    stage: &mut Stage,
    label: LabelId,
    cond: &TirExpr<'core>,
    block: &TirBlock<'core>,
  ) -> Port {
    let (mut layer, mut cond_stage) = self.child_layer(stage);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(cond_stage.interface),
      break_value: None,
    });

    let (mut then_stage, else_stage) = self.distill_cond(&mut layer, &mut cond_stage, cond);

    let result = self.distill_block(&mut then_stage, block);
    self.drop(block.span, &mut then_stage, block.ty, result);
    then_stage.transfer = Some(Transfer::unconditional(cond_stage.interface));

    self.finish_stage(cond_stage);
    self.finish_stage(then_stage);
    self.finish_stage(else_stage);
    self.finish_layer(layer);

    Port::Erase
  }

  pub(super) fn distill_if(
    &mut self,
    stage: &mut Stage,
    arms: &[(TirExpr<'core>, TirBlock<'core>)],
    leg: &Option<TirBlock<'core>>,
  ) -> Port {
    let local = self.new_local(stage);
    let (mut layer, mut cur_stage) = self.child_layer(stage);

    for (cond, block) in arms {
      let (mut then_stage, else_stage) = self.distill_cond(&mut layer, &mut cur_stage, cond);
      let result = self.distill_block(&mut then_stage, block);
      then_stage.set_local_to(local, result);
      self.finish_stage(cur_stage);
      self.finish_stage(then_stage);
      cur_stage = else_stage;
    }

    if let Some(leg) = leg {
      let result = self.distill_block(&mut cur_stage, leg);
      cur_stage.set_local_to(local, result);
    } else {
      cur_stage.erase_local(local);
    }

    self.finish_stage(cur_stage);
    self.finish_layer(layer);

    stage.take_local(local)
  }

  pub(super) fn distill_do(
    &mut self,
    stage: &mut Stage,
    label: LabelId,
    block: &TirBlock<'core>,
  ) -> Port {
    let local = self.new_local(stage);
    let (layer, mut body_stage) = self.child_layer(stage);

    *self.labels.get_or_extend(label) =
      Some(Label { layer: layer.id, continue_transfer: None, break_value: Some(local) });

    let result = self.distill_block(&mut body_stage, block);
    body_stage.set_local_to(local, result);

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.take_local(local)
  }

  pub(super) fn distill_cond_bool(
    &mut self,
    stage: &mut Stage,
    cond: &TirExpr<'core>,
    negate: bool,
  ) -> Port {
    match &cond.kind {
      TirExprKind![!cond] => {
        let value = self.distill_expr_value(stage, cond);
        if negate {
          let wire = stage.new_wire();
          stage.steps.push(Step::ExtFn("n32_eq", false, value, Port::N32(0), wire.0));
          wire.1
        } else {
          value
        }
      }
      TirExprKind::Bool(b) => Port::N32((*b ^ negate) as u32),
      TirExprKind::Not(inner) => self.distill_cond_bool(stage, inner, !negate),
      _ => {
        let local = self.new_local(stage);
        let (mut sub_layer, mut sub_stage) = self.child_layer(stage);
        let (mut true_stage, mut false_stage) =
          self.distill_cond(&mut sub_layer, &mut sub_stage, cond);
        true_stage.set_local_to(local, Port::N32(if negate { 0 } else { 1 }));
        false_stage.set_local_to(local, Port::N32(if negate { 1 } else { 0 }));
        self.finish_stage(true_stage);
        self.finish_stage(false_stage);
        self.finish_stage(sub_stage);
        self.finish_layer(sub_layer);
        stage.take_local(local)
      }
    }
  }

  pub(super) fn distill_cond(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    cond: &TirExpr<'core>,
  ) -> (Stage, Stage) {
    match &cond.kind {
      TirExprKind![!cond] => {
        let bool = self.distill_expr_value(stage, cond);
        let interface = self.interfaces.push(None);
        let true_stage = self.new_stage(layer, interface);
        let false_stage = self.new_stage(layer, interface);
        self.interfaces[interface] = Some(Interface::new(
          interface,
          layer.id,
          InterfaceKind::Branch(false_stage.id, true_stage.id),
        ));
        stage.transfer = Some(Transfer { interface, data: Some(bool) });
        (true_stage, false_stage)
      }
      TirExprKind::Bool(bool) => {
        let true_stage = self.new_unconditional_stage(layer);
        let false_stage = self.new_unconditional_stage(layer);
        stage.transfer = Some(Transfer::unconditional(if *bool {
          true_stage.interface
        } else {
          false_stage.interface
        }));
        (true_stage, false_stage)
      }
      TirExprKind::Not(inner) => swap(self.distill_cond(layer, stage, inner)),
      TirExprKind::Is(value, pat) => {
        let value = self.distill_expr_value(stage, value);
        let true_stage = self.new_unconditional_stage(layer);
        let false_stage = self.new_unconditional_stage(layer);
        self.distill_pattern_match(
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
        let (mut a_true, mut a_false) = swap_if(invert_a, self.distill_cond(layer, stage, a));
        let (b_true, mut b_false) = swap_if(invert_b, self.distill_cond(layer, &mut a_true, b));
        let true_stage = b_true;
        let false_stage = self.new_unconditional_stage(layer);
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
  if bool {
    (b, a)
  } else {
    (a, b)
  }
}
