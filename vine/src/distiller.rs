use std::mem::replace;

use ivm::ext::ExtFnKind;
use vine_util::idx::{Counter, IdxVec};

use crate::{
  ast::{Block, Expr, ExprKind, GenericPath, LabelId, Local, Pat, PatKind},
  vir::{Interface, InterfaceId, Layer, LayerId, LocalUse, Port, Stage, StageId, Step, Transfer},
};

#[derive(Default)]
pub struct Distiller {
  layers: IdxVec<LayerId, Option<Layer>>,
  interfaces: IdxVec<InterfaceId, Option<Interface>>,
  stages: IdxVec<StageId, Option<Stage>>,

  labels: IdxVec<LabelId, Option<Label>>,
  returns: Vec<Return>,
}

struct Label {
  layer: LayerId,
  continue_transfer: Option<InterfaceId>,
  break_value: Option<Local>,
}

struct Return {
  layer: LayerId,
  local: Local,
}

impl Distiller {
  fn new_stage(&mut self, interface: InterfaceId) -> Stage {
    let id = self.stages.push(None);
    Stage { id, interface, steps: Vec::new(), transfer: None, wire: Counter::default() }
  }

  fn child_layer(&mut self, stage: &mut Stage) -> (Layer, Stage) {
    todo!()
  }

  fn finish_stage(&mut self, stage: Stage) {
    let id = stage.id;
    self.stages[id] = Some(stage);
  }

  fn finish_layer(&mut self, layer: Layer) {
    todo!()
  }

  fn new_interface_mono(&mut self) {
    let interface_id = self.interfaces.push(None);
    let stage = self.new_stage(interface_id);
  }

  fn distill_block(&mut self, stage: &mut Stage, block: &Block) -> Port {
    todo!()
  }

  fn new_local(&mut self) -> Local {
    todo!()
  }

  pub fn distill_expr_value(&mut self, stage: &mut Stage, expr: &Expr) -> Port {
    match &expr.kind {
      ExprKind![sugar || error || !value] => unreachable!("{expr:?}"),

      ExprKind![cond] => self.distill_cond_bool(stage, expr, false),

      ExprKind::Paren(inner) => self.distill_expr_value(stage, inner),
      ExprKind::Char(c) => Port::N32(*c as u32),
      ExprKind::N32(n) => Port::N32(*n),
      ExprKind::F32(f) => Port::F32(*f),

      ExprKind::Path(generic_path) => todo!(),
      ExprKind::DynFn(dyn_fn_id) => todo!(),
      ExprKind::Do(label, block) => todo!(),
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
      ExprKind::Ref(place) => {
        let (value, space) = self.distill_expr_place(stage, place);
        let wire = stage.new_wire();
        stage.steps.push(Step::Ref(wire.0, value, space));
        wire.1
      }
      ExprKind::Move(place) => {
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
      ExprKind::Inverse(inner) => self.distill_expr_space(stage, inner),
      ExprKind::Tuple(tuple) => self.list(stage, tuple, Self::distill_expr_value, Step::Tuple),
      ExprKind::Adt(path, args) => self.list(stage, args, Self::distill_expr_value, adt(path)),
      ExprKind::List(list) => self.list(stage, list, Self::distill_expr_value, Step::List),
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
        stage.steps.push(Step::ExtFn(ExtFnKind::sub.into(), wire.0, Port::N32(0), value));
        wire.1
      }
      ExprKind::String(s) => {
        let wire = stage.new_wire();
        stage.steps.push(Step::String(wire.0, s.clone()));
        wire.1
      }
      ExprKind::CopyLocal(local) => {
        let wire = stage.new_wire();
        stage.steps.push(Step::Local(*local, LocalUse::Get(wire.0)));
        wire.1
      }
      ExprKind::MoveLocal(local) => {
        let wire = stage.new_wire();
        stage.steps.push(Step::Local(*local, LocalUse::Take(wire.0)));
        wire.1
      }

      ExprKind::BinaryOp(binary_op, expr, expr1) => todo!(),
      ExprKind::ComparisonOp(expr, vec) => todo!(),
      ExprKind::BinaryOpAssign(binary_op, expr, expr1) => todo!(),

      ExprKind::If(cond, then_branch, else_branch) => {
        let local = self.new_local();
        let (mut layer, mut cond_stage) = self.child_layer(stage);
        let (mut then_stage, mut else_stage) = self.distill_cond(&mut layer, &mut cond_stage, cond);

        let result = self.distill_block(&mut then_stage, then_branch);
        then_stage.steps.push(Step::Local(local, LocalUse::Set(result)));

        let result = self.distill_expr_value(&mut else_stage, else_branch);
        else_stage.steps.push(Step::Local(local, LocalUse::Set(result)));

        self.finish_stage(cond_stage);
        self.finish_stage(then_stage);
        self.finish_stage(else_stage);
        self.finish_layer(layer);

        let wire = stage.new_wire();
        stage.steps.push(Step::Local(local, LocalUse::Take(wire.0)));
        wire.1
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
        then_stage.transfer = Some(Transfer { interface: cond_stage.interface, data: None });

        self.finish_stage(cond_stage);
        self.finish_stage(then_stage);
        self.finish_stage(else_stage);
        self.finish_layer(layer);

        Port::Erase
      }

      ExprKind::Loop(label, block) => {
        let local = self.new_local();
        let (layer, mut body_stage) = self.child_layer(stage);

        *self.labels.get_or_extend(label.as_id()) = Some(Label {
          layer: layer.id,
          continue_transfer: Some(body_stage.interface),
          break_value: Some(local),
        });

        let result = self.distill_block(&mut body_stage, block);
        body_stage.erase(result);
        body_stage.transfer = Some(Transfer { interface: body_stage.interface, data: None });

        self.finish_stage(body_stage);
        self.finish_layer(layer);

        let wire = stage.new_wire();
        stage.steps.push(Step::Local(local, LocalUse::Take(wire.0)));
        wire.1
      }

      ExprKind::Return(value) => {
        let value =
          value.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);
        let return_ = self.returns.last().unwrap();

        stage.steps.push(Step::Local(return_.local, LocalUse::Set(value)));
        stage.steps.push(Step::Diverge(return_.layer, None));

        Port::Erase
      }

      ExprKind::Break(label, value) => {
        let value =
          value.as_ref().map(|v| self.distill_expr_value(stage, v)).unwrap_or(Port::Erase);

        let label = self.labels[label.as_id()].as_ref().unwrap();
        if let Some(local) = label.break_value {
          stage.steps.push(Step::Local(local, LocalUse::Set(value)));
        } else {
          stage.erase(value);
        }

        stage.steps.push(Step::Diverge(label.layer, None));

        Port::Erase
      }

      ExprKind::Continue(label) => {
        let label = self.labels[label.as_id()].as_ref().unwrap();
        let transfer = Transfer { interface: label.continue_transfer.unwrap(), data: None };
        stage.steps.push(Step::Diverge(label.layer, Some(transfer)));
        Port::Erase
      }

      ExprKind::Match(expr, vec) => todo!(),
      ExprKind::Fn(vec, ty, expr) => todo!(),
    }
  }

  fn distill_expr_space(&mut self, stage: &mut Stage, expr: &Expr) -> Port {
    match &expr.kind {
      ExprKind![sugar || error || !space] => unreachable!("{expr:?}"),
      ExprKind::Paren(inner) => self.distill_expr_space(stage, inner),
      ExprKind::Hole => Port::Erase,
      ExprKind::Inverse(inner) => self.distill_expr_value(stage, inner),
      ExprKind::Tuple(tuple) => self.list(stage, tuple, Self::distill_expr_space, Step::Tuple),
      ExprKind::Adt(path, args) => self.list(stage, args, Self::distill_expr_space, adt(path)),
      ExprKind::Set(place) => {
        let (value, space) = self.distill_expr_place(stage, place);
        stage.erase(value);
        space
      }
      ExprKind::SetLocal(local) => {
        let wire = stage.new_wire();
        stage.steps.push(Step::Local(*local, LocalUse::Set(wire.0)));
        wire.1
      }
    }
  }

  fn distill_expr_place(&mut self, stage: &mut Stage, expr: &Expr) -> (Port, Port) {
    match &expr.kind {
      ExprKind![sugar || error || !place] => unreachable!("{expr:?}"),
      ExprKind::Paren(inner) => self.distill_expr_place(stage, inner),
      ExprKind::Local(local) => {
        let value = stage.new_wire();
        let space = stage.new_wire();
        stage.steps.push(Step::Local(*local, LocalUse::Mut(value.0, space.0)));
        (value.1, space.1)
      }
      ExprKind::Deref(reference) => {
        let reference = self.distill_expr_value(stage, reference);
        let value = stage.new_wire();
        let space = stage.new_wire();
        stage.steps.push(Step::Ref(reference, value.0, space.0));
        (value.1, space.1)
      }
      ExprKind::Inverse(inner) => {
        let (value, space) = self.distill_expr_place(stage, inner);
        (space, value)
      }
      ExprKind::Place(value, space) => {
        (self.distill_expr_value(stage, value), self.distill_expr_space(stage, space))
      }
      ExprKind::Tuple(tuple) => self.list_pair(stage, tuple, Self::distill_expr_place, Step::Tuple),
      ExprKind::Adt(path, args) => self.list_pair(stage, args, Self::distill_expr_place, adt(path)),
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

  pub fn distill_pat_value(&mut self, stage: &mut Stage, pat: &Pat) -> Port {
    match &pat.kind {
      PatKind![!value] => unreachable!(),
      PatKind::Hole => Port::Erase,
      PatKind::Paren(inner) => self.distill_pat_value(stage, inner),
      PatKind::Inverse(inner) => self.distill_pat_space(stage, inner),
      PatKind::Local(local) => {
        let wire = stage.new_wire();
        stage.steps.push(Step::Local(*local, LocalUse::Set(wire.0)));
        wire.1
      }
      PatKind::Tuple(tuple) => self.list(stage, tuple, Self::distill_pat_value, Step::Tuple),
      PatKind::Adt(path, args) => {
        self.list(stage, args.as_deref().unwrap_or(&[]), Self::distill_pat_value, adt(path))
      }
      PatKind::Ref(place) => {
        let (value, space) = self.distill_pat_place(stage, place);
        let wire = stage.new_wire();
        stage.steps.push(Step::Ref(wire.0, value, space));
        wire.1
      }
    }
  }

  fn distill_pat_space(&mut self, stage: &mut Stage, pat: &Pat) -> Port {
    match &pat.kind {
      PatKind![!space] => unreachable!(),
      PatKind::Hole => Port::Erase,
      PatKind::Paren(inner) => self.distill_pat_space(stage, inner),
      PatKind::Inverse(inner) => self.distill_pat_value(stage, inner),
      PatKind::Local(local) => {
        let wire = stage.new_wire();
        stage.steps.push(Step::Local(*local, LocalUse::Take(wire.0)));
        wire.1
      }
      PatKind::Tuple(tuple) => self.list(stage, tuple, Self::distill_pat_space, Step::Tuple),
      PatKind::Adt(path, args) => {
        self.list(stage, args.as_deref().unwrap_or(&[]), Self::distill_pat_space, adt(path))
      }
    }
  }

  fn distill_pat_place(&mut self, stage: &mut Stage, pat: &Pat) -> (Port, Port) {
    match &pat.kind {
      PatKind![!place] => unreachable!(),
      PatKind::Hole => stage.new_wire(),
      PatKind::Paren(inner) => self.distill_pat_place(stage, inner),
      PatKind::Inverse(inner) => {
        let (value, space) = self.distill_pat_place(stage, inner);
        (space, value)
      }
      PatKind::Local(local) => {
        let value = stage.new_wire();
        let space = stage.new_wire();
        stage.steps.push(Step::Local(*local, LocalUse::Mut(space.0, value.0)));
        (value.1, space.1)
      }
      PatKind::Tuple(tuple) => self.list_pair(stage, tuple, Self::distill_pat_place, Step::Tuple),
      PatKind::Adt(path, args) => {
        self.list_pair(stage, args.as_deref().unwrap_or(&[]), Self::distill_pat_place, adt(path))
      }
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
      PatKind::Move(value) => {
        let value = self.distill_pat_value(stage, value);
        (value, Port::Erase)
      }
    }
  }

  fn distill_cond_bool(&mut self, stage: &mut Stage, cond: &Expr, negate: bool) -> Port {
    match &cond.kind {
      ExprKind![!cond] => {
        let value = self.distill_expr_value(stage, cond);
        if negate {
          let wire = stage.new_wire();
          stage.steps.push(Step::ExtFn(ExtFnKind::eq.into(), wire.0, value, Port::N32(0)));
          wire.1
        } else {
          value
        }
      }
      ExprKind::Bool(b) => Port::N32(*b as u32),
      ExprKind::Not(inner) => self.distill_cond_bool(stage, inner, !negate),
      ExprKind::Is(expr, pat) => todo!(),
      ExprKind::LogicalOp(logical_op, expr, expr1) => todo!(),
    }
  }

  fn distill_cond(&mut self, layer: &mut Layer, stage: &mut Stage, cond: &Expr) -> (Stage, Stage) {
    todo!()
  }

  fn list<T>(
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

  fn list_pair<T>(
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
