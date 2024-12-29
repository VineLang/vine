mod pattern_matching;

use std::mem::replace;

use ivm::ext::{ExtFn, ExtFnKind};
use pattern_matching::Row;
use vine_util::idx::{Counter, IdxVec};

use crate::{
  ast::{
    BinaryOp, Block, ComparisonOp, DynFnId, Expr, ExprKind, GenericPath, LabelId, Local, LogicalOp,
    Pat, PatKind, Span, StmtKind, Ty,
  },
  resolver::{DefId, Resolver},
  vir::{
    Interface, InterfaceId, InterfaceKind, Layer, LayerId, Port, Stage, StageId, Step, Transfer,
  },
};

#[derive(Debug)]
pub struct Distiller<'core, 'r> {
  resolver: &'r Resolver<'core>,

  layers: IdxVec<LayerId, Option<Layer>>,
  interfaces: IdxVec<InterfaceId, Option<Interface>>,
  stages: IdxVec<StageId, Option<Stage>>,

  labels: IdxVec<LabelId, Option<Label>>,
  returns: Vec<Return>,
  dyn_fns: IdxVec<DynFnId, Option<DynFn>>,

  concat: DefId,

  locals: Counter<Local>,
}

#[derive(Debug)]
struct Label {
  layer: LayerId,
  continue_transfer: Option<InterfaceId>,
  break_value: Option<Local>,
}

#[derive(Debug)]
struct Return {
  layer: LayerId,
  local: Local,
}

#[derive(Debug)]
struct DynFn {
  interface: InterfaceId,
  local: Local,
}

impl<'core, 'r> Distiller<'core, 'r> {
  fn new_stage(&mut self, layer: &mut Layer, interface: InterfaceId) -> Stage {
    let id = self.stages.push(None);
    layer.stages.push(id);
    Stage {
      id,
      layer: layer.id,
      interface,
      header: Vec::new(),
      declarations: Vec::new(),
      steps: Vec::new(),
      transfer: None,
      wires: Counter::default(),
    }
  }

  fn new_layer(&mut self) -> Layer {
    let id = self.layers.push(None);
    Layer { id, parent: None, stages: Vec::new() }
  }

  fn child_layer(&mut self, parent_stage: &mut Stage) -> (Layer, Stage) {
    let mut layer = self.new_layer();
    layer.parent = Some(parent_stage.layer);
    let stage = self.new_unconditional_stage(&mut layer);
    (layer, stage)
  }

  fn root_layer(&mut self) -> (Layer, Stage) {
    let mut layer = self.new_layer();
    let stage = self.new_unconditional_stage(&mut layer);
    (layer, stage)
  }

  fn finish_stage(&mut self, stage: Stage) {
    let id = stage.id;
    self.stages[id] = Some(stage);
  }

  fn finish_layer(&mut self, layer: Layer) {
    let id = layer.id;
    self.layers[id] = Some(layer);
  }

  fn new_unconditional_stage(&mut self, layer: &mut Layer) -> Stage {
    let interface = self.interfaces.push(None);
    let stage = self.new_stage(layer, interface);
    self.interfaces[interface] =
      Some(Interface::new(interface, layer.id, InterfaceKind::Unconditional(stage.id)));
    stage
  }

  fn new_local(&mut self, stage: &mut Stage) -> Local {
    let local = self.locals.next();
    stage.declarations.push(local);
    local
  }

  fn distill_block(&mut self, stage: &mut Stage, block: &Block<'core>) -> Port {
    let mut result = Port::Erase;
    for stmt in &block.stmts {
      stage.erase(result);
      result = Port::Erase;
      match &stmt.kind {
        StmtKind::Let(let_) => {
          let pat = self.distill_pat_value(stage, &let_.bind);
          let value =
            let_.init.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);
          stage.steps.push(Step::Link(pat, value));
        }
        StmtKind::DynFn(dyn_fn) => {
          let local = self.new_local(stage);
          let (layer, mut stage) = self.root_layer();
          *self.dyn_fns.get_or_extend(dyn_fn.id.unwrap()) =
            Some(DynFn { interface: stage.interface, local });
          self._distill_fn(&mut stage, local, &dyn_fn.params, &dyn_fn.body, Self::distill_block);
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

  pub fn distill_expr_value(&mut self, stage: &mut Stage, expr: &Expr<'core>) -> Port {
    match &expr.kind {
      ExprKind![sugar || error || !value] => unreachable!("{expr:?}"),

      ExprKind![cond] => self.distill_cond_bool(stage, expr, false),

      ExprKind::Paren(inner) => self.distill_expr_value(stage, inner),
      ExprKind::Char(c) => Port::N32(*c as u32),
      ExprKind::N32(n) => Port::N32(*n),
      ExprKind::F32(f) => Port::F32(*f),

      ExprKind::Path(path) => Port::Const(path.path.resolved.unwrap()),
      ExprKind::DynFn(dyn_fn) => {
        let dyn_fn = self.dyn_fns[*dyn_fn].as_ref().unwrap();
        stage.steps.push(Step::Transfer(Transfer::unconditional(dyn_fn.interface)));
        stage.get_local(dyn_fn.local)
      }
      ExprKind::Block(block) => self.distill_block(stage, block),
      ExprKind::Assign(inverse, space, value) => {
        if *inverse {
          let space = self.distill_expr_space(stage, space);
          let value = self.distill_expr_space(stage, value);
          stage.steps.push(Step::Link(space, value));
        } else {
          let value = self.distill_expr_space(stage, value);
          let space = self.distill_expr_space(stage, space);
          stage.steps.push(Step::Link(space, value));
        }
        Port::Erase
      }
      ExprKind::Ref(place, _) => {
        let (value, space) = self.distill_expr_place(stage, place);
        let wire = stage.new_wire();
        stage.steps.push(Step::Ref(wire.0, value, space));
        wire.1
      }
      ExprKind::Move(place, _) => {
        let (value, space) = self.distill_expr_place(stage, place);
        stage.erase(space);
        value
      }
      ExprKind::Copy(place) => {
        let (value, space) = self.distill_expr_place(stage, place);
        let wire = stage.new_wire();
        stage.steps.push(Step::Dup(value, wire.0, space));
        wire.1
      }
      ExprKind::Inverse(inner, _) => self.distill_expr_space(stage, inner),
      ExprKind::Tuple(tuple) => {
        self.distill_vec(stage, tuple, Self::distill_expr_value, Step::Tuple)
      }
      ExprKind::Adt(path, args) => {
        self.distill_vec(stage, args, Self::distill_expr_value, adt(path))
      }
      ExprKind::List(list) => self.distill_vec(stage, list, Self::distill_expr_value, Step::List),
      ExprKind::TupleField(tuple, idx, len) => {
        let tuple = self.distill_expr_value(stage, tuple);
        let len = len.unwrap();
        let mut ports = (0..len).map(|_| Port::Erase).collect::<Vec<_>>();
        let wire = stage.new_wire();
        ports[*idx] = wire.0;
        stage.steps.push(Step::Tuple(tuple, ports));
        wire.1
      }
      ExprKind::Call(func, args) => {
        let func = self.distill_expr_value(stage, func);
        let args = args.iter().map(|s| self.distill_expr_value(stage, s)).collect::<Vec<_>>();
        let wire = stage.new_wire();
        stage.steps.push(Step::Fn(func, args, wire.0));
        wire.1
      }
      ExprKind::Neg(value) => {
        let value = self.distill_expr_value(stage, value);
        let wire = stage.new_wire();
        stage.steps.push(Step::ExtFn(
          ExtFn::from(ExtFnKind::sub).swap(),
          value,
          Port::N32(0),
          wire.0,
        ));
        wire.1
      }
      ExprKind::String(s) => {
        let wire = stage.new_wire();
        stage.steps.push(Step::String(wire.0, s.clone()));
        wire.1
      }
      ExprKind::CopyLocal(local) => stage.get_local(*local),
      ExprKind::MoveLocal(local) => stage.take_local(*local),
      ExprKind::BinaryOp(op, lhs, rhs) => {
        let lhs = self.distill_expr_value(stage, lhs);
        let rhs = self.distill_expr_value(stage, rhs);
        let out = stage.new_wire();
        self.distill_bin_op(stage, *op, lhs, rhs, out.0);
        out.1
      }
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        let (lhs, out) = self.distill_expr_place(stage, lhs);
        let rhs = self.distill_expr_value(stage, rhs);
        self.distill_bin_op(stage, *op, lhs, rhs, out);
        Port::Erase
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut last_result = Port::Erase;
        let mut lhs = self.distill_expr_value(stage, init);
        for (i, (op, rhs)) in cmps.iter().enumerate() {
          let ext_fn = match op {
            ComparisonOp::Eq => ExtFn::from(ExtFnKind::eq),
            ComparisonOp::Ne => ExtFn::from(ExtFnKind::ne),
            ComparisonOp::Lt => ExtFn::from(ExtFnKind::lt),
            ComparisonOp::Gt => ExtFn::from(ExtFnKind::lt).swap(),
            ComparisonOp::Le => ExtFn::from(ExtFnKind::le),
            ComparisonOp::Ge => ExtFn::from(ExtFnKind::le).swap(),
          };
          let first = i == 0;
          let last = i == cmps.len() - 1;
          let rhs = self.distill_expr_value(stage, rhs);
          let (rhs, next_lhs) = if last { (rhs, Port::Erase) } else { stage.dup(rhs) };
          let result = stage.ext_fn(ext_fn, lhs, rhs);
          last_result = if first {
            result
          } else {
            stage.ext_fn(ExtFnKind::n32_and.into(), last_result, result)
          };
          lhs = next_lhs;
        }
        last_result
      }

      ExprKind::Do(label, block) => {
        let local = self.new_local(stage);
        let (layer, mut body_stage) = self.child_layer(stage);

        *self.labels.get_or_extend(label.as_id()) =
          Some(Label { layer: layer.id, continue_transfer: None, break_value: Some(local) });

        let result = self.distill_block(&mut body_stage, block);
        body_stage.set_local_to(local, result);

        self.finish_stage(body_stage);
        self.finish_layer(layer);

        stage.take_local(local)
      }

      ExprKind::If(cond, then_branch, else_branch) => {
        let local = self.new_local(stage);
        let (mut layer, mut cond_stage) = self.child_layer(stage);
        let (mut then_stage, mut else_stage) = self.distill_cond(&mut layer, &mut cond_stage, cond);

        let result = self.distill_block(&mut then_stage, then_branch);
        then_stage.set_local_to(local, result);

        let result = self.distill_expr_value(&mut else_stage, else_branch);
        else_stage.set_local_to(local, result);

        self.finish_stage(cond_stage);
        self.finish_stage(then_stage);
        self.finish_stage(else_stage);
        self.finish_layer(layer);

        stage.take_local(local)
      }

      ExprKind::While(label, cond, block) => {
        let (mut layer, mut cond_stage) = self.child_layer(stage);

        *self.labels.get_or_extend(label.as_id()) = Some(Label {
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

      ExprKind::Loop(label, block) => {
        let local = self.new_local(stage);
        let (layer, mut body_stage) = self.child_layer(stage);

        *self.labels.get_or_extend(label.as_id()) = Some(Label {
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

      ExprKind::Return(value) => {
        let value =
          value.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);
        let return_ = self.returns.last().unwrap();

        stage.set_local_to(return_.local, value);
        stage.steps.push(Step::Diverge(return_.layer, None));

        Port::Erase
      }

      ExprKind::Break(label, value) => {
        let value =
          value.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);

        let label = self.labels[label.as_id()].as_ref().unwrap();
        if let Some(local) = label.break_value {
          stage.set_local_to(local, value);
        } else {
          stage.erase(value);
        }

        stage.steps.push(Step::Diverge(label.layer, None));

        Port::Erase
      }

      ExprKind::Continue(label) => {
        let label = self.labels[label.as_id()].as_ref().unwrap();
        let transfer = Transfer::unconditional(label.continue_transfer.unwrap());
        stage.steps.push(Step::Diverge(label.layer, Some(transfer)));
        Port::Erase
      }

      ExprKind::Fn(params, _, body) => {
        let fn_local = self.new_local(stage);
        let (layer, mut body_stage) = self.child_layer(stage);

        self._distill_fn(&mut body_stage, fn_local, params, &**body, Self::distill_expr_value);

        self.finish_stage(body_stage);
        self.finish_layer(layer);

        stage.get_local(fn_local)
      }

      ExprKind::Match(value, arms) => {
        let local = self.new_local(stage);
        let value = self.distill_expr_value(stage, value);
        let (mut layer, mut init_stage) = self.child_layer(stage);
        let rows = arms
          .iter()
          .map(|(pat, expr)| {
            let mut stage = self.new_unconditional_stage(&mut layer);
            let result = self.distill_expr_value(&mut stage, expr);
            stage.set_local_to(local, result);
            let interface = stage.interface;
            self.finish_stage(stage);
            Row::new(pat, interface)
          })
          .collect();
        self.distill_match(&mut layer, &mut init_stage, value, rows);
        self.finish_stage(init_stage);
        self.finish_layer(layer);
        stage.take_local(local)
      }
    }
  }

  fn distill_expr_space(&mut self, stage: &mut Stage, expr: &Expr<'core>) -> Port {
    match &expr.kind {
      ExprKind![sugar || error || !space] => unreachable!("{expr:?}"),
      ExprKind::Paren(inner) => self.distill_expr_space(stage, inner),
      ExprKind::Hole => Port::Erase,
      ExprKind::Inverse(inner, _) => self.distill_expr_value(stage, inner),
      ExprKind::Tuple(tuple) => {
        self.distill_vec(stage, tuple, Self::distill_expr_space, Step::Tuple)
      }
      ExprKind::Adt(path, args) => {
        self.distill_vec(stage, args, Self::distill_expr_space, adt(path))
      }
      ExprKind::Set(place) => {
        let (value, space) = self.distill_expr_place(stage, place);
        stage.erase(value);
        space
      }
      ExprKind::SetLocal(local) => stage.set_local(*local),
    }
  }

  fn distill_expr_place(&mut self, stage: &mut Stage, expr: &Expr<'core>) -> (Port, Port) {
    match &expr.kind {
      ExprKind![sugar || error || !place] => unreachable!("{expr:?}"),
      ExprKind::Paren(inner) => self.distill_expr_place(stage, inner),
      ExprKind::Local(local) => stage.mut_local(*local),
      ExprKind::Deref(reference, _) => {
        let reference = self.distill_expr_value(stage, reference);
        let value = stage.new_wire();
        let space = stage.new_wire();
        stage.steps.push(Step::Ref(reference, value.0, space.0));
        (value.1, space.1)
      }
      ExprKind::Inverse(inner, _) => {
        let (value, space) = self.distill_expr_place(stage, inner);
        (space, value)
      }
      ExprKind::Place(value, space) => {
        (self.distill_expr_value(stage, value), self.distill_expr_space(stage, space))
      }
      ExprKind::Tuple(tuple) => {
        self.distill_vec_pair(stage, tuple, Self::distill_expr_place, Step::Tuple)
      }
      ExprKind::Adt(path, args) => {
        self.distill_vec_pair(stage, args, Self::distill_expr_place, adt(path))
      }
      ExprKind::TupleField(tuple, idx, len) => {
        let tuple = self.distill_expr_place(stage, tuple);
        let len = len.unwrap();
        let (mut values, spaces) = (0..len).map(|_| stage.new_wire()).collect::<(Vec<_>, Vec<_>)>();
        let wire = stage.new_wire();
        let value = wire.0;
        let space = replace(&mut values[*idx], wire.1);
        stage.steps.push(Step::Tuple(tuple.0, values));
        stage.steps.push(Step::Tuple(tuple.1, spaces));
        (value, space)
      }
    }
  }

  pub fn distill_pat_value(&mut self, stage: &mut Stage, pat: &Pat<'core>) -> Port {
    match &pat.kind {
      PatKind![!value] => unreachable!(),
      PatKind::Hole => Port::Erase,
      PatKind::Paren(inner) => self.distill_pat_value(stage, inner),
      PatKind::Inverse(inner) => self.distill_pat_space(stage, inner),
      PatKind::Local(local) => stage.set_local(*local),
      PatKind::Tuple(tuple) => self.distill_vec(stage, tuple, Self::distill_pat_value, Step::Tuple),
      PatKind::Adt(path, args) => {
        self.distill_vec(stage, args.as_deref().unwrap_or(&[]), Self::distill_pat_value, adt(path))
      }
      PatKind::Ref(place) => {
        let (value, space) = self.distill_pat_place(stage, place);
        let wire = stage.new_wire();
        stage.steps.push(Step::Ref(wire.0, value, space));
        wire.1
      }
    }
  }

  fn distill_pat_space(&mut self, stage: &mut Stage, pat: &Pat<'core>) -> Port {
    match &pat.kind {
      PatKind![!space] => unreachable!(),
      PatKind::Hole => Port::Erase,
      PatKind::Paren(inner) => self.distill_pat_space(stage, inner),
      PatKind::Inverse(inner) => self.distill_pat_value(stage, inner),
      PatKind::Local(local) => stage.take_local(*local),
      PatKind::Tuple(tuple) => self.distill_vec(stage, tuple, Self::distill_pat_space, Step::Tuple),
      PatKind::Adt(path, args) => {
        self.distill_vec(stage, args.as_deref().unwrap_or(&[]), Self::distill_pat_space, adt(path))
      }
    }
  }

  fn distill_pat_place(&mut self, stage: &mut Stage, pat: &Pat<'core>) -> (Port, Port) {
    match &pat.kind {
      PatKind![!place] => unreachable!(),
      PatKind::Hole => stage.new_wire(),
      PatKind::Paren(inner) => self.distill_pat_place(stage, inner),
      PatKind::Inverse(inner) => {
        let (value, space) = self.distill_pat_place(stage, inner);
        (space, value)
      }
      PatKind::Local(local) => {
        let (a, b) = stage.mut_local(*local);
        (b, a)
      }
      PatKind::Tuple(tuple) => {
        self.distill_vec_pair(stage, tuple, Self::distill_pat_place, Step::Tuple)
      }
      PatKind::Adt(path, args) => self.distill_vec_pair(
        stage,
        args.as_deref().unwrap_or(&[]),
        Self::distill_pat_place,
        adt(path),
      ),
      PatKind::Ref(place) => {
        let (value_0, value_1) = self.distill_pat_place(stage, place);
        let ref_in = stage.new_wire();
        let ref_out = stage.new_wire();
        let value_2 = stage.new_wire();
        stage.steps.push(Step::Ref(ref_in.0, value_0, value_2.0));
        stage.steps.push(Step::Ref(ref_out.0, value_1, value_2.1));
        (ref_in.1, ref_out.1)
      }
      PatKind::Deref(reference) => {
        let reference = self.distill_pat_value(stage, reference);
        let value = stage.new_wire();
        let space = stage.new_wire();
        stage.steps.push(Step::Ref(reference, value.0, space.0));
        (value.1, space.1)
      }
    }
  }

  fn distill_bin_op(&mut self, stage: &mut Stage, op: BinaryOp, lhs: Port, rhs: Port, out: Port) {
    let ext_fn = match op {
      BinaryOp::Concat => {
        stage.steps.push(Step::Fn(Port::Const(self.concat), vec![lhs, rhs], out));
        return;
      }
      BinaryOp::BitOr => ExtFnKind::n32_or,
      BinaryOp::BitXor => ExtFnKind::n32_xor,
      BinaryOp::BitAnd => ExtFnKind::n32_and,
      BinaryOp::Shl => ExtFnKind::n32_shl,
      BinaryOp::Shr => ExtFnKind::n32_shr,
      BinaryOp::Add => ExtFnKind::add,
      BinaryOp::Sub => ExtFnKind::sub,
      BinaryOp::Mul => ExtFnKind::mul,
      BinaryOp::Div => ExtFnKind::div,
      BinaryOp::Rem => ExtFnKind::rem,
    };
    stage.steps.push(Step::ExtFn(ext_fn.into(), lhs, rhs, out));
  }

  fn distill_cond_bool(&mut self, stage: &mut Stage, cond: &Expr<'core>, negate: bool) -> Port {
    if let ExprKind::Paren(inner) = &cond.kind {
      return self.distill_cond_bool(stage, inner, negate);
    }
    match &cond.kind {
      ExprKind![!cond] => {
        let value = self.distill_expr_value(stage, cond);
        if negate {
          let wire = stage.new_wire();
          stage.steps.push(Step::ExtFn(ExtFnKind::eq.into(), value, Port::N32(0), wire.0));
          wire.1
        } else {
          value
        }
      }
      ExprKind::Bool(b) => Port::N32(*b as u32),
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

  fn distill_cond(
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
            return swap(self.distill_cond(layer, stage, lhs));
          } else if *op == ComparisonOp::Eq {
            return self.distill_cond(layer, stage, lhs);
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
        self.distill_match(
          layer,
          stage,
          value,
          vec![
            Row::new(pat, true_stage.interface),
            Row::new(&Pat { span: Span::NONE, kind: PatKind::Hole }, false_stage.interface),
          ],
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

  fn _distill_fn<B>(
    &mut self,
    stage: &mut Stage,
    fn_local: Local,
    params: &[(Pat<'core>, Option<Ty<'core>>)],
    body: &B,
    distill_body: impl Fn(&mut Self, &mut Stage, &B) -> Port,
  ) {
    let return_local = self.new_local(stage);

    let params = params.iter().map(|(p, _)| self.distill_pat_value(stage, p)).collect::<Vec<_>>();

    let result = stage.take_local(return_local);
    let wire = stage.new_wire();
    stage.steps.push(Step::Fn(wire.0, params, result));
    stage.set_local_to(fn_local, wire.1);

    self.returns.push(Return { layer: stage.layer, local: return_local });

    let result = distill_body(self, stage, body);
    stage.set_local_to(return_local, result);

    self.returns.pop();
  }

  fn distill_vec<T>(
    &mut self,
    stage: &mut Stage,
    tuple: &[T],
    mut f: impl FnMut(&mut Self, &mut Stage, &T) -> Port,
    s: impl Fn(Port, Vec<Port>) -> Step,
  ) -> Port {
    let ports = tuple.iter().map(|x| f(self, stage, x)).collect::<Vec<_>>();
    let wire = stage.new_wire();
    stage.steps.push(s(wire.0, ports));
    wire.1
  }

  fn distill_vec_pair<T>(
    &mut self,
    stage: &mut Stage,
    tuple: &[T],
    mut f: impl FnMut(&mut Self, &mut Stage, &T) -> (Port, Port),
    s: impl Fn(Port, Vec<Port>) -> Step,
  ) -> (Port, Port) {
    let (lefts, rights) = tuple.iter().map(|x| f(self, stage, x)).collect::<(Vec<_>, Vec<_>)>();
    let left = stage.new_wire();
    let right = stage.new_wire();
    stage.steps.push(s(left.0, lefts));
    stage.steps.push(s(right.0, rights));
    (left.1, right.1)
  }
}

fn adt(path: &GenericPath) -> impl Fn(Port, Vec<Port>) -> Step {
  let adt = path.path.resolved.unwrap();
  move |x, y| Step::Adt(adt, x, y)
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
