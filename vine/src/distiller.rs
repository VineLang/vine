use std::{
  collections::BTreeMap,
  mem::{replace, take},
};

use vine_util::{
  idx::{Counter, IdxVec},
  unwrap_idx_vec,
};

use crate::{
  analyzer::usage::Usage,
  ast::{BinaryOp, ComparisonOp, DynFnId, Expr, ExprKind, Key, LabelId, Local, Pat, PatKind},
  chart::{AdtId, Chart, ValueDef, ValueDefKind, VariantId},
  vir::{
    Interface, InterfaceId, InterfaceKind, Layer, LayerId, Port, Stage, StageId, Step, Transfer,
    VIR,
  },
};

mod control_flow;
mod pattern_matching;

#[derive(Debug)]
pub struct Distiller<'core, 'r> {
  chart: &'r Chart<'core>,

  layers: IdxVec<LayerId, Option<Layer>>,
  interfaces: IdxVec<InterfaceId, Option<Interface>>,
  stages: IdxVec<StageId, Option<Stage>>,

  labels: IdxVec<LabelId, Option<Label>>,
  returns: Vec<Return>,
  dyn_fns: IdxVec<DynFnId, Option<DynFn>>,

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
  pub fn new(chart: &'r Chart<'core>) -> Self {
    Distiller {
      chart,
      layers: Default::default(),
      interfaces: Default::default(),
      stages: Default::default(),
      labels: Default::default(),
      returns: Default::default(),
      dyn_fns: Default::default(),
      locals: Default::default(),
    }
  }

  pub fn distill(&mut self, value_def: &ValueDef<'core>) -> Option<VIR> {
    match &value_def.kind {
      ValueDefKind::Taken => unreachable!(),
      ValueDefKind::Const { value, .. } => {
        Some(self.distill_root(value_def.locals, |self_, stage, local| {
          let result = self_.distill_expr_value(stage, value);
          stage.set_local_to(local, result);
        }))
      }
      ValueDefKind::Fn { params, body, .. } => {
        Some(self.distill_root(value_def.locals, |self_, stage, local| {
          self_._distill_fn(stage, local, params, body);
        }))
      }
      ValueDefKind::Ivy { .. } | ValueDefKind::Adt(..) | ValueDefKind::TraitSubitem(..) => None,
    }
  }

  pub fn distill_root(
    &mut self,
    locals: Counter<Local>,
    f: impl FnOnce(&mut Self, &mut Stage, Local),
  ) -> VIR {
    self.locals = locals;
    let (layer, mut stage) = self.root_layer();
    let local = self.locals.next();
    f(self, &mut stage, local);
    self.finish_stage(stage);
    self.finish_layer(layer);
    self.labels.clear();
    debug_assert!(self.returns.is_empty());
    self.dyn_fns.clear();
    VIR {
      layers: unwrap_idx_vec(take(&mut self.layers)),
      interfaces: unwrap_idx_vec(take(&mut self.interfaces)),
      stages: unwrap_idx_vec(take(&mut self.stages)),
      locals: self.locals,
      globals: vec![(local, Usage::Take)],
    }
  }

  fn distill_expr_value(&mut self, stage: &mut Stage, expr: &Expr<'core>) -> Port {
    match &expr.kind {
      ExprKind![sugar || error || !value] => unreachable!("{expr:?}"),

      ExprKind![cond] => self.distill_cond_bool(stage, expr, false),
      ExprKind::Do(label, block) => self.distill_do(stage, label.as_id(), block),
      ExprKind::If(arms, leg) => self.distill_if(stage, arms, leg),
      ExprKind::While(label, cond, block) => self.distill_while(stage, label.as_id(), cond, block),
      ExprKind::Loop(label, block) => self.distill_loop(stage, label.as_id(), block),
      ExprKind::Return(value) => self.distill_return(stage, value),
      ExprKind::Break(label, value) => self.distill_break(stage, label.as_id(), value),
      ExprKind::Continue(label) => self.distill_continue(stage, label.as_id()),
      ExprKind::Fn(params, _, body) => self.distill_fn(stage, params, body),
      ExprKind::Match(value, arms) => self.distill_match(stage, value, arms),

      ExprKind::Paren(inner) => self.distill_expr_value(stage, inner),
      ExprKind::Char(c) => Port::N32(*c as u32),
      ExprKind::N32(n) => Port::N32(*n),
      ExprKind::F32(f) => Port::F32(*f),

      ExprKind::Path(_) => unreachable!(),
      ExprKind::Def(id, _) => Port::Def(*id),
      ExprKind::Rel(id) => Port::Rel(*id),

      ExprKind::DynFn(dyn_fn) => {
        let dyn_fn = self.dyn_fns[*dyn_fn].as_ref().unwrap();
        stage.steps.push(Step::Transfer(Transfer::unconditional(dyn_fn.interface)));
        stage.take_local(dyn_fn.local)
      }
      ExprKind::Assign(inverse, space, value) => {
        if *inverse {
          let space = self.distill_expr_space(stage, space);
          let value = self.distill_expr_value(stage, value);
          stage.steps.push(Step::Link(space, value));
        } else {
          let value = self.distill_expr_value(stage, value);
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
      ExprKind::Object(fields) => self.distill_object(stage, fields, Self::distill_expr_value),
      ExprKind::Adt(adt_id, variant_id, _, args) => {
        self.distill_vec(stage, args, Self::distill_expr_value, adt(adt_id, variant_id))
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
        stage.steps.push(Step::ExtFn("sub", true, value, Port::N32(0), wire.0));
        wire.1
      }
      ExprKind::String(init, rest) => {
        let wire = stage.new_wire();
        let rest = rest
          .iter()
          .map(|(expr, seg)| (self.distill_expr_value(stage, expr), seg.content.clone()))
          .collect();
        stage.steps.push(Step::String(wire.0, init.content.clone(), rest));
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
        let rhs = self.distill_expr_value(stage, rhs);
        let (lhs, out) = self.distill_expr_place(stage, lhs);
        self.distill_bin_op(stage, *op, lhs, rhs, out);
        Port::Erase
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut last_result = Port::Erase;
        let mut lhs = self.distill_expr_value(stage, init);
        for (i, (op, rhs)) in cmps.iter().enumerate() {
          let (ext_fn, swap) = match op {
            ComparisonOp::Eq => ("eq", false),
            ComparisonOp::Ne => ("ne", false),
            ComparisonOp::Lt => ("lt", false),
            ComparisonOp::Gt => ("lt", true),
            ComparisonOp::Le => ("le", false),
            ComparisonOp::Ge => ("le", true),
          };
          let first = i == 0;
          let last = i == cmps.len() - 1;
          let rhs = self.distill_expr_value(stage, rhs);
          let (rhs, next_lhs) = if last { (rhs, Port::Erase) } else { stage.dup(rhs) };
          let result = stage.ext_fn(ext_fn, swap, lhs, rhs);
          last_result =
            if first { result } else { stage.ext_fn("n32_and", false, last_result, result) };
          lhs = next_lhs;
        }
        last_result
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
      ExprKind::Object(fields) => self.distill_object(stage, fields, Self::distill_expr_space),
      ExprKind::Adt(adt_id, variant_id, _, args) => {
        self.distill_vec(stage, args, Self::distill_expr_space, adt(adt_id, variant_id))
      }
      ExprKind::Set(place) => {
        let (value, space) = self.distill_expr_place(stage, place);
        stage.erase(value);
        space
      }
      ExprKind::Hedge(place) => {
        let (value, space) = self.distill_expr_place(stage, place);
        let wire = stage.new_wire();
        stage.steps.push(Step::Dup(space, wire.0, value));
        wire.1
      }
      ExprKind::SetLocal(local) => stage.set_local(*local),
      ExprKind::HedgeLocal(local) => stage.hedge_local(*local),
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
      ExprKind::Object(fields) => self.distill_object_pair(stage, fields, Self::distill_expr_place),
      ExprKind::Adt(adt_id, variant_id, _, args) => {
        self.distill_vec_pair(stage, args, Self::distill_expr_place, adt(adt_id, variant_id))
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

  fn distill_pat_value(&mut self, stage: &mut Stage, pat: &Pat<'core>) -> Port {
    match &pat.kind {
      PatKind![!value] | PatKind::PathCall(..) => unreachable!(),
      PatKind::Hole => Port::Erase,
      PatKind::Paren(inner) | PatKind::Annotation(inner, _) => self.distill_pat_value(stage, inner),
      PatKind::Inverse(inner) => self.distill_pat_space(stage, inner),
      PatKind::Local(local) => {
        stage.declarations.push(*local);
        stage.set_local(*local)
      }
      PatKind::Tuple(tuple) => self.distill_vec(stage, tuple, Self::distill_pat_value, Step::Tuple),
      PatKind::Object(fields) => self.distill_object(stage, fields, Self::distill_pat_value),
      PatKind::Adt(adt_id, variant_id, _, args) => {
        self.distill_vec(stage, args, Self::distill_pat_value, adt(adt_id, variant_id))
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
      PatKind![!space] | PatKind::PathCall(..) => unreachable!(),
      PatKind::Hole => Port::Erase,
      PatKind::Paren(inner) | PatKind::Annotation(inner, _) => self.distill_pat_space(stage, inner),
      PatKind::Inverse(inner) => self.distill_pat_value(stage, inner),
      PatKind::Local(local) => {
        stage.declarations.push(*local);
        stage.take_local(*local)
      }
      PatKind::Tuple(tuple) => self.distill_vec(stage, tuple, Self::distill_pat_space, Step::Tuple),
      PatKind::Object(fields) => self.distill_object(stage, fields, Self::distill_pat_space),
      PatKind::Adt(adt_id, variant_id, _, args) => {
        self.distill_vec(stage, args, Self::distill_pat_space, adt(adt_id, variant_id))
      }
    }
  }

  fn distill_pat_place(&mut self, stage: &mut Stage, pat: &Pat<'core>) -> (Port, Port) {
    match &pat.kind {
      PatKind![!place] | PatKind::PathCall(..) => unreachable!(),
      PatKind::Hole => stage.new_wire(),
      PatKind::Paren(inner) | PatKind::Annotation(inner, _) => self.distill_pat_place(stage, inner),
      PatKind::Inverse(inner) => {
        let (value, space) = self.distill_pat_place(stage, inner);
        (space, value)
      }
      PatKind::Local(local) => {
        stage.declarations.push(*local);
        let (a, b) = stage.mut_local(*local);
        (b, a)
      }
      PatKind::Tuple(tuple) => {
        self.distill_vec_pair(stage, tuple, Self::distill_pat_place, Step::Tuple)
      }
      PatKind::Object(fields) => self.distill_object_pair(stage, fields, Self::distill_pat_place),
      PatKind::Adt(adt_id, variant_id, _, args) => {
        self.distill_vec_pair(stage, args, Self::distill_pat_place, adt(adt_id, variant_id))
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
    }
  }

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
    parent_stage.steps.push(Step::Transfer(Transfer::unconditional(stage.interface)));
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

  fn distill_bin_op(&mut self, stage: &mut Stage, op: BinaryOp, lhs: Port, rhs: Port, out: Port) {
    let ext_fn = match op {
      BinaryOp::Concat => {
        stage.steps.push(Step::Fn(
          Port::Def(self.chart.builtins.concat.unwrap()),
          vec![lhs, rhs],
          out,
        ));
        return;
      }
      BinaryOp::BitOr => "n32_or",
      BinaryOp::BitXor => "n32_xor",
      BinaryOp::BitAnd => "n32_and",
      BinaryOp::Shl => "n32_shl",
      BinaryOp::Shr => "n32_shr",
      BinaryOp::Add => "add",
      BinaryOp::Sub => "sub",
      BinaryOp::Mul => "mul",
      BinaryOp::Div => "div",
      BinaryOp::Rem => "rem",
    };
    stage.steps.push(Step::ExtFn(ext_fn, false, lhs, rhs, out));
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

  fn distill_object<T>(
    &mut self,
    stage: &mut Stage,
    fields: &Vec<(Key<'core>, T)>,
    mut f: impl FnMut(&mut Self, &mut Stage, &T) -> Port,
  ) -> Port {
    let ports = fields
      .iter()
      .map(|(k, v)| (k.ident, f(self, stage, v)))
      .collect::<BTreeMap<_, _>>()
      .into_values()
      .collect::<Vec<_>>();
    let wire = stage.new_wire();
    stage.steps.push(Step::Tuple(wire.0, ports));
    wire.1
  }

  fn distill_object_pair<T>(
    &mut self,
    stage: &mut Stage,
    fields: &Vec<(Key<'core>, T)>,
    mut f: impl FnMut(&mut Self, &mut Stage, &T) -> (Port, Port),
  ) -> (Port, Port) {
    let (lefts, rights) = fields
      .iter()
      .map(|(k, v)| (k.ident, f(self, stage, v)))
      .collect::<BTreeMap<_, _>>()
      .into_values()
      .collect::<(Vec<_>, Vec<_>)>();
    let left = stage.new_wire();
    let right = stage.new_wire();
    stage.steps.push(Step::Tuple(left.0, lefts));
    stage.steps.push(Step::Tuple(right.0, rights));
    (left.1, right.1)
  }
}

fn adt(&adt: &AdtId, &variant: &VariantId) -> impl Fn(Port, Vec<Port>) -> Step {
  move |x, y| Step::Adt(adt, variant, x, y)
}
