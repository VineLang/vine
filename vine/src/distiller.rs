use std::mem::{replace, take};

use vine_util::{
  idx::{Counter, IdxVec},
  unwrap_idx_vec,
};

use crate::{
  analyzer::usage::Usage,
  chart::Chart,
  core::Core,
  diag::Diag,
  tir::{ClosureId, LabelId, Local, TirExpr, TirExprKind, TirPat, TirPatKind},
  vir::{
    Header, Interface, InterfaceId, InterfaceKind, Layer, LayerId, Port, Stage, StageId, Step,
    Transfer, VIR,
  },
};

mod control_flow;
mod pattern_matching;

#[derive(Debug)]
pub struct Distiller<'core, 'r> {
  chart: &'r Chart<'core>,
  core: &'core Core<'core>,

  layers: IdxVec<LayerId, Option<Layer>>,
  interfaces: IdxVec<InterfaceId, Option<Interface>>,
  stages: IdxVec<StageId, Option<Stage>>,

  labels: IdxVec<LabelId, Option<Label>>,
  returns: Vec<Return>,
  closures: IdxVec<ClosureId, Closure>,

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
struct Closure {
  interface: InterfaceId,
}

impl<'core, 'r> Distiller<'core, 'r> {
  // pub fn new(chart: &'r Chart<'core>) -> Self {
  //   Distiller {
  //     chart,
  //     layers: Default::default(),
  //     interfaces: Default::default(),
  //     stages: Default::default(),
  //     labels: Default::default(),
  //     returns: Default::default(),
  //     dyn_fns: Default::default(),
  //     locals: Default::default(),
  //   }
  // }

  // pub fn distill(&mut self, value_def: &ValueDef<'core>) -> Option<VIR> {
  //   match &value_def.kind {
  //     ValueDefKind::Taken => unreachable!(),
  //     ValueDefKind::Const { value, .. } => {
  //       Some(self.distill_root(value_def.locals, |self_, stage, local| {
  //         let result = self_.distill_expr_value(stage, value);
  //         stage.set_local_to(local, result);
  //       }))
  //     }
  //     ValueDefKind::Fn { params, body, .. } => {
  //       Some(self.distill_root(value_def.locals, |self_, stage, local| {
  //         self_._distill_fn(stage, local, params, body);
  //       }))
  //     }
  //     ValueDefKind::Adt(..) | ValueDefKind::TraitSubitem(..) => None,
  //   }
  // }

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
    self.closures.clear();
    VIR {
      layers: unwrap_idx_vec(take(&mut self.layers)),
      interfaces: unwrap_idx_vec(take(&mut self.interfaces)),
      stages: unwrap_idx_vec(take(&mut self.stages)),
      locals: self.locals,
      globals: vec![(local, Usage::Take)],
    }
  }

  fn distill_expr_value(&mut self, stage: &mut Stage, expr: &TirExpr<'core>) -> Port {
    let span = expr.span;
    match &expr.kind {
      TirExprKind::Error(e) => Port::Error(*e),
      TirExprKind![!value && place] => todo!(),
      TirExprKind![!value && !place && space] => {
        Port::Error(self.core.report(Diag::ExpectedValueFoundSpaceExpr { span }))
      }

      TirExprKind![cond] => self.distill_cond_bool(stage, expr, false),
      TirExprKind::Do(label, block) => self.distill_do(stage, *label, block),
      TirExprKind::If(arms, leg) => self.distill_if(stage, arms, leg),
      TirExprKind::While(label, cond, block) => self.distill_while(stage, *label, cond, block),
      TirExprKind::Loop(label, block) => self.distill_loop(stage, *label, block),
      TirExprKind::Return(value) => self.distill_return(stage, value),
      TirExprKind::Break(label, value) => self.distill_break(stage, *label, value),
      TirExprKind::Continue(label) => self.distill_continue(stage, *label),
      TirExprKind::Match(value, arms) => self.distill_match(stage, value, arms),
      TirExprKind::Char(c) => Port::N32(*c as u32),
      TirExprKind::N32(n) => Port::N32(*n),
      TirExprKind::F32(f) => Port::F32(*f),
      TirExprKind::Def(id, _) => Port::Def(*id),
      TirExprKind::Assign(inverse, space, value) => {
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
      TirExprKind::Ref(place) => {
        let place = self.distill_expr_place(stage, place);
        stage.ref_place(place)
      }
      TirExprKind::Inverse(inner) => self.distill_expr_space(stage, inner),
      TirExprKind::Composite(els) => {
        self.distill_vec(stage, els, Self::distill_expr_value, Step::Composite)
      }
      TirExprKind::Struct(_, data) => self.distill_expr_value(stage, data),
      TirExprKind::Enum(enum_id, variant_id, data) => {
        let data = data.as_ref().map(|e| self.distill_expr_value(stage, e));
        let wire = stage.new_wire();
        stage.steps.push(Step::Enum(wire.0, *enum_id, *variant_id, data));
        wire.1
      }
      TirExprKind::List(list) => {
        self.distill_vec(stage, list, Self::distill_expr_value, Step::List)
      }
      TirExprKind::Field(composite, idx, len) => {
        let composite = self.distill_expr_value(stage, composite);
        let mut ports = (0..*len).map(|_| Port::Erase).collect::<Vec<_>>();
        let wire = stage.new_wire();
        ports[*idx] = wire.0;
        stage.steps.push(Step::Composite(composite, ports));
        wire.1
      }
      TirExprKind::Call(impl_, func, args) => {
        let func = func.as_ref().map(|f| self.distill_expr_value(stage, f)).unwrap_or(Port::Erase);
        let args = args.iter().map(|s| self.distill_expr_value(stage, s)).collect::<Vec<_>>();
        let wire = stage.new_wire();
        stage.steps.push(Step::Call(impl_.clone(), func, args, wire.0));
        wire.1
      }
      TirExprKind::String(init, rest) => {
        let wire = stage.new_wire();
        let rest = rest
          .iter()
          .map(|(expr, seg)| (self.distill_expr_value(stage, expr), seg.clone()))
          .collect();
        stage.steps.push(Step::String(wire.0, init.clone(), rest));
        wire.1
      }
      TirExprKind::CallAssign(impl_, lhs, rhs) => {
        let rhs = self.distill_expr_value(stage, rhs);
        let (lhs, out) = self.distill_expr_place(stage, lhs);
        stage.steps.push(Step::Call(impl_.clone(), Port::Erase, vec![lhs, rhs], out));
        Port::Erase
      }
      TirExprKind::CallCompare(init, cmps) => {
        let mut last_result = Port::Erase;
        let lhs = self.distill_expr_place(stage, init);
        let mut lhs = stage.ref_place(lhs);
        for (i, (impl_, rhs)) in cmps.iter().enumerate() {
          let first = i == 0;
          let last = i == cmps.len() - 1;
          let rhs = self.distill_expr_place(stage, rhs);
          let (rhs, next_lhs) = if last {
            (stage.ref_place(rhs), Port::Erase)
          } else {
            let wire = stage.new_wire();
            (stage.ref_place((rhs.0, wire.0)), stage.ref_place((wire.1, rhs.1)))
          };
          let result = stage.new_wire();
          stage.steps.push(Step::Call(impl_.clone(), Port::Erase, vec![lhs, rhs], result.0));
          last_result =
            if first { result.1 } else { stage.ext_fn("n32_and", false, last_result, result.1) };
          lhs = next_lhs;
        }
        last_result
      }
      TirExprKind::InlineIvy(binds, net) => {
        let out = stage.new_wire();
        let binds = binds
          .iter()
          .map(|(var, value, expr)| {
            (
              var.0 .0.into(),
              if *value {
                self.distill_expr_value(stage, expr)
              } else {
                self.distill_expr_space(stage, expr)
              },
            )
          })
          .collect();
        stage.steps.push(Step::InlineIvy(binds, out.0, net.clone()));
        out.1
      }
      TirExprKind::Local(local) => stage.get_local(*local),
      TirExprKind::Unwrap(inner) => self.distill_expr_value(stage, inner),

      TirExprKind::Closure(closure_id) => {
        let wire = stage.new_wire();
        let closure = &self.closures[*closure_id];
        stage
          .steps
          .push(Step::Transfer(Transfer { interface: closure.interface, data: Some(wire.0) }));
        wire.1
      }
    }
  }

  fn distill_expr_space(&mut self, stage: &mut Stage, expr: &TirExpr<'core>) -> Port {
    let span = expr.span;
    match &expr.kind {
      TirExprKind::Error(e) => Port::Error(*e),
      TirExprKind![!space && place] => todo!(),
      TirExprKind![!space && !place && value] => {
        Port::Error(self.core.report(Diag::ExpectedSpaceFoundValueExpr { span }))
      }

      TirExprKind::Hole => Port::Erase,
      TirExprKind::Inverse(inner) => self.distill_expr_value(stage, inner),
      TirExprKind::Composite(els) => {
        self.distill_vec(stage, els, Self::distill_expr_space, Step::Composite)
      }
      TirExprKind::Struct(_, data) => self.distill_expr_space(stage, data),
      TirExprKind::Local(local) => stage.hedge_local(*local),
      TirExprKind::Unwrap(inner) => self.distill_expr_space(stage, inner),
    }
  }

  fn distill_expr_place(&mut self, stage: &mut Stage, expr: &TirExpr<'core>) -> (Port, Port) {
    match &expr.kind {
      TirExprKind::Error(e) => (Port::Error(*e), Port::Error(*e)),
      TirExprKind![!place && value && !space] => todo!(),
      TirExprKind![!place && space && !value] => todo!(),
      TirExprKind::Local(local) => stage.mut_local(*local),
      TirExprKind::Deref(reference) => {
        let reference = self.distill_expr_value(stage, reference);
        let value = stage.new_wire();
        let space = stage.new_wire();
        stage.steps.push(Step::Ref(reference, value.0, space.0));
        (value.1, space.1)
      }
      TirExprKind::Inverse(inner) => {
        let (value, space) = self.distill_expr_place(stage, inner);
        (space, value)
      }
      TirExprKind::Place(value, space) => {
        (self.distill_expr_value(stage, value), self.distill_expr_space(stage, space))
      }
      TirExprKind::Composite(els) => {
        self.distill_vec_pair(stage, els, Self::distill_expr_place, Step::Composite)
      }
      TirExprKind::Struct(_, data) => self.distill_expr_place(stage, data),
      TirExprKind::Field(tuple, idx, len) => {
        let tuple = self.distill_expr_place(stage, tuple);
        let (mut values, spaces) =
          (0..*len).map(|_| stage.new_wire()).collect::<(Vec<_>, Vec<_>)>();
        let wire = stage.new_wire();
        let value = wire.0;
        let space = replace(&mut values[*idx], wire.1);
        stage.steps.push(Step::Composite(tuple.0, values));
        stage.steps.push(Step::Composite(tuple.1, spaces));
        (value, space)
      }
      TirExprKind::Unwrap(inner) => self.distill_expr_place(stage, inner),
    }
  }

  fn distill_pat_nil(&mut self, stage: &mut Stage, pat: &TirPat) {
    match &pat.kind {
      TirPatKind::Hole | TirPatKind::Enum(_, _, None) | TirPatKind::Error(_) => {}
      TirPatKind::Composite(els) => {
        els.iter().for_each(|e| self.distill_pat_nil(stage, e));
      }
      TirPatKind::Struct(_, inner)
      | TirPatKind::Enum(_, _, Some(inner))
      | TirPatKind::Ref(inner)
      | TirPatKind::Deref(inner)
      | TirPatKind::Inverse(inner) => {
        self.distill_pat_nil(stage, inner);
      }
      TirPatKind::Local(local) => {
        stage.declarations.push(*local);
        stage.erase_local(*local);
      }
    }
  }

  fn distill_pat_value(&mut self, stage: &mut Stage, pat: &TirPat) -> Port {
    let span = pat.span;
    match &pat.kind {
      TirPatKind::Error(e) => Port::Error(*e),
      TirPatKind::Deref(_) => Port::Error(self.core.report(Diag::DerefNonPlacePat { span })),
      TirPatKind::Enum(..) => Port::Error(self.core.report(Diag::ExpectedCompletePat { span })),
      TirPatKind::Hole => Port::Erase,
      TirPatKind::Inverse(inner) => self.distill_pat_space(stage, inner),
      TirPatKind::Local(local) => {
        stage.declarations.push(*local);
        stage.set_local(*local)
      }
      TirPatKind::Composite(els) => {
        self.distill_vec(stage, els, Self::distill_pat_value, Step::Composite)
      }
      TirPatKind::Struct(_, data) => self.distill_pat_value(stage, data),
      TirPatKind::Ref(place) => {
        let (value, space) = self.distill_pat_place(stage, place);
        let wire = stage.new_wire();
        stage.steps.push(Step::Ref(wire.0, value, space));
        wire.1
      }
    }
  }

  fn distill_pat_space(&mut self, stage: &mut Stage, pat: &TirPat) -> Port {
    let span = pat.span;
    match &pat.kind {
      TirPatKind::Error(e) => Port::Error(*e),
      TirPatKind::Deref(_) => Port::Error(self.core.report(Diag::DerefNonPlacePat { span })),
      TirPatKind::Ref(_) => Port::Error(self.core.report(Diag::RefSpacePat { span })),
      TirPatKind::Enum(..) => Port::Error(self.core.report(Diag::ExpectedCompletePat { span })),
      TirPatKind::Hole => Port::Erase,
      TirPatKind::Inverse(inner) => self.distill_pat_value(stage, inner),
      TirPatKind::Local(local) => {
        stage.declarations.push(*local);
        stage.take_local(*local)
      }
      TirPatKind::Composite(els) => {
        self.distill_vec(stage, els, Self::distill_pat_space, Step::Composite)
      }
      TirPatKind::Struct(_, data) => self.distill_pat_space(stage, data),
    }
  }

  fn distill_pat_place(&mut self, stage: &mut Stage, pat: &TirPat) -> (Port, Port) {
    let span = pat.span;
    match &pat.kind {
      TirPatKind::Error(e) => (Port::Error(*e), Port::Error(*e)),
      TirPatKind::Enum(..) => {
        let e = self.core.report(Diag::ExpectedCompletePat { span });
        (Port::Error(e), Port::Error(e))
      }
      TirPatKind::Hole => stage.new_wire(),
      TirPatKind::Inverse(inner) => {
        let (value, space) = self.distill_pat_place(stage, inner);
        (space, value)
      }
      TirPatKind::Local(local) => {
        stage.declarations.push(*local);
        let (a, b) = stage.mut_local(*local);
        (b, a)
      }
      TirPatKind::Composite(els) => {
        self.distill_vec_pair(stage, els, Self::distill_pat_place, Step::Composite)
      }
      TirPatKind::Struct(_, data) => self.distill_pat_place(stage, data),
      TirPatKind::Ref(place) => {
        let (value_0, value_1) = self.distill_pat_place(stage, place);
        let ref_in = stage.new_wire();
        let ref_out = stage.new_wire();
        let value_2 = stage.new_wire();
        stage.steps.push(Step::Ref(ref_in.0, value_0, value_2.0));
        stage.steps.push(Step::Ref(ref_out.0, value_1, value_2.1));
        (ref_in.1, ref_out.1)
      }
      TirPatKind::Deref(reference) => {
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
      header: Header::None,
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

  fn finish_stage(&mut self, stage: Stage) -> StageId {
    let id = stage.id;
    self.stages[id] = Some(stage);
    id
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
