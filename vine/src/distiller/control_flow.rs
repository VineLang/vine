use crate::{
  ast::{Block, ComparisonOp, Expr, ExprKind, LabelId, Local, LogicalOp, Pat, Stmt, StmtKind},
  vir::{Interface, InterfaceKind, Layer, Port, Stage, Step, Transfer},
};

use super::{pattern_matching::Row, Distiller, DynFn, Label, Return};

impl<'core, 'r> Distiller<'core, 'r> {
  pub fn distill_block(&mut self, stage: &mut Stage, block: &Block<'core>) -> Port {
    self.distill_stmts(stage, &block.stmts)
  }

  fn distill_stmts(&mut self, stage: &mut Stage, stmts: &[Stmt<'core>]) -> Port {
    let mut result = Port::Erase;
    for (i, stmt) in stmts.iter().enumerate() {
      stage.erase(result);
      result = Port::Erase;
      match &stmt.kind {
        StmtKind::Let(let_) => {
          if let Some(else_block) = &let_.else_block {
            let local = self.new_local(stage);
            stage.erase_local(local);
            let (mut layer, mut init_stage) = self.child_layer(stage);
            let value = let_
              .init
              .as_ref()
              .map(|v| self.distill_expr_value(&mut init_stage, v))
              .unwrap_or(Port::Erase);
            let mut then_stage = self.new_unconditional_stage(&mut layer);
            let mut else_stage = self.new_unconditional_stage(&mut layer);
            self.distill_pattern_match(
              &mut layer,
              &mut init_stage,
              value,
              vec![
                Row::new(&let_.bind, then_stage.interface),
                Row::new(&Pat::HOLE, else_stage.interface),
              ],
            );
            let result = self.distill_stmts(&mut then_stage, &stmts[i + 1..]);
            then_stage.set_local_to(local, result);
            let result = self.distill_block(&mut else_stage, else_block);
            else_stage.erase(result);
            self.finish_stage(init_stage);
            self.finish_stage(then_stage);
            self.finish_stage(else_stage);
            self.finish_layer(layer);
            return stage.take_local(local);
          } else {
            let value =
              let_.init.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);
            let pat = self.distill_pat_value(stage, &let_.bind);
            stage.steps.push(Step::Link(pat, value));
          }
        }
        StmtKind::DynFn(dyn_fn) => {
          let local = self.new_local(stage);
          stage.erase_local(local);
          let (layer, mut stage) = self.root_layer();
          *self.dyn_fns.get_or_extend(dyn_fn.id.unwrap()) =
            Some(DynFn { interface: stage.interface, local });
          self._distill_fn(&mut stage, local, &dyn_fn.params, &dyn_fn.body);
          self.finish_stage(stage);
          self.finish_layer(layer);
        }
        StmtKind::Expr(expr, semi) => {
          result = self.distill_expr_value(stage, expr);
          if *semi {
            stage.erase(result);
            result = Port::Erase;
          }
        }
        StmtKind::Item(_) | StmtKind::Empty => {}
      }
    }
    result
  }

  pub(super) fn distill_match(
    &mut self,
    stage: &mut Stage,
    value: &Expr<'core>,
    arms: &Vec<(Pat<'core>, Block<'core>)>,
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
        Row::new(pat, interface)
      })
      .collect();
    self.distill_pattern_match(&mut layer, &mut init_stage, value, rows);
    self.finish_stage(init_stage);
    self.finish_layer(layer);
    stage.take_local(local)
  }

  pub(super) fn distill_fn(
    &mut self,
    stage: &mut Stage,
    params: &[Pat<'core>],
    body: &Block<'core>,
  ) -> Port {
    let fn_local = self.new_local(stage);
    let (layer, mut body_stage) = self.child_layer(stage);

    self._distill_fn(&mut body_stage, fn_local, params, body);

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.take_local(fn_local)
  }

  pub(super) fn _distill_fn(
    &mut self,
    stage: &mut Stage,
    fn_local: Local,
    params: &[Pat<'core>],
    body: &Block<'core>,
  ) {
    let return_local = self.new_local(stage);

    let params = params.iter().map(|p| self.distill_pat_value(stage, p)).collect::<Vec<_>>();

    let result = stage.take_local(return_local);
    let wire = stage.new_wire();
    stage.steps.push(Step::Fn(wire.0, params, result));
    stage.set_local_to(fn_local, wire.1);

    self.returns.push(Return { layer: stage.layer, local: return_local });

    let result = self.distill_block(stage, body);
    stage.set_local_to(return_local, result);

    self.returns.pop();
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
    value: &Option<Box<Expr<'core>>>,
  ) -> Port {
    let value = value.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);

    let label = self.labels[label].as_ref().unwrap();
    if let Some(local) = label.break_value {
      stage.set_local_to(local, value);
    } else {
      stage.erase(value);
    }

    stage.steps.push(Step::Diverge(label.layer, None));

    Port::Erase
  }

  pub(super) fn distill_return(
    &mut self,
    stage: &mut Stage,
    value: &Option<Box<Expr<'core>>>,
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
    block: &Block<'core>,
  ) -> Port {
    let local = self.new_local(stage);
    let (layer, mut body_stage) = self.child_layer(stage);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(body_stage.interface),
      break_value: Some(local),
    });

    let result = self.distill_block(&mut body_stage, block);
    body_stage.erase(result);
    body_stage.transfer = Some(Transfer::unconditional(body_stage.interface));

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.take_local(local)
  }

  pub(super) fn distill_while(
    &mut self,
    stage: &mut Stage,
    label: LabelId,
    cond: &Expr<'core>,
    block: &Block<'core>,
  ) -> Port {
    let (mut layer, mut cond_stage) = self.child_layer(stage);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(cond_stage.interface),
      break_value: None,
    });

    let (mut then_stage, else_stage) = self.distill_cond(&mut layer, &mut cond_stage, cond);

    let result = self.distill_block(&mut then_stage, block);
    then_stage.erase(result);
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
    arms: &[(Expr<'core>, Block<'core>)],
    leg: &Option<Block<'core>>,
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
    block: &Block<'core>,
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
    cond: &Expr<'core>,
    negate: bool,
  ) -> Port {
    if let ExprKind::Paren(inner) = &cond.kind {
      return self.distill_cond_bool(stage, inner, negate);
    }
    match &cond.kind {
      ExprKind![!cond] => {
        let value = self.distill_expr_value(stage, cond);
        if negate {
          let wire = stage.new_wire();
          stage.steps.push(Step::ExtFn("eq", false, value, Port::N32(0), wire.0));
          wire.1
        } else {
          value
        }
      }
      ExprKind::Bool(b) => Port::N32((*b ^ negate) as u32),
      ExprKind::Not(inner) => self.distill_cond_bool(stage, inner, !negate),
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
    cond: &Expr<'core>,
  ) -> (Stage, Stage) {
    if let ExprKind::Paren(inner) = &cond.kind {
      return self.distill_cond(layer, stage, inner);
    }
    if let ExprKind::ComparisonOp(lhs, cmps) = &cond.kind {
      if let [(op, rhs)] = &**cmps {
        if matches!(rhs.kind, ExprKind::N32(0)) {
          if *op == ComparisonOp::Ne {
            return self.distill_cond(layer, stage, lhs);
          } else if *op == ComparisonOp::Eq {
            return swap(self.distill_cond(layer, stage, lhs));
          }
        }
      }
    }
    match &cond.kind {
      ExprKind![!cond] => {
        let bool = self.distill_expr_value(stage, cond);
        let interface = self.interfaces.push(None);
        let true_stage = self.new_stage(layer, interface);
        let false_stage = self.new_stage(layer, interface);
        self.interfaces[interface] = Some(Interface::new(
          interface,
          layer.id,
          InterfaceKind::Branch([false_stage.id, true_stage.id]),
        ));
        stage.transfer = Some(Transfer { interface, data: Some(bool) });
        (true_stage, false_stage)
      }
      ExprKind::Bool(bool) => {
        let true_stage = self.new_unconditional_stage(layer);
        let false_stage = self.new_unconditional_stage(layer);
        stage.transfer = Some(Transfer::unconditional(if *bool {
          true_stage.interface
        } else {
          false_stage.interface
        }));
        (true_stage, false_stage)
      }
      ExprKind::Not(inner) => swap(self.distill_cond(layer, stage, inner)),
      ExprKind::Is(value, pat) => {
        let value = self.distill_expr_value(stage, value);
        let true_stage = self.new_unconditional_stage(layer);
        let false_stage = self.new_unconditional_stage(layer);
        self.distill_pattern_match(
          layer,
          stage,
          value,
          vec![Row::new(pat, true_stage.interface), Row::new(&Pat::HOLE, false_stage.interface)],
        );
        (true_stage, false_stage)
      }
      ExprKind::LogicalOp(op, a, b) => {
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
