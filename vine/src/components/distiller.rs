use std::mem::take;

use vine_util::{
  idx::{Counter, IdxVec},
  unwrap_idx_vec,
};

use crate::structures::{
  ast::Span,
  chart::{Chart, DefId, GenericsId},
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  resolutions::{Fragment, Rels},
  signatures::Signatures,
  tir::{ClosureId, Local, TargetId, TirExpr, TirExprKind, TirLocal, TirPat, TirPatKind},
  types::{Type, Types},
  vir::{
    Header, Interface, InterfaceId, InterfaceKind, Layer, LayerId, Port, Stage, StageId, Step,
    Transfer, Vir, VirLocal,
  },
};

#[derive(Debug)]
pub struct Distiller<'core, 'r> {
  pub(crate) core: &'core Core<'core>,
  pub(crate) chart: &'r Chart<'core>,
  pub(crate) sigs: &'r Signatures<'core>,

  pub(crate) layers: IdxVec<LayerId, Option<Layer>>,
  pub(crate) interfaces: IdxVec<InterfaceId, Option<Interface>>,
  pub(crate) stages: IdxVec<StageId, Option<Stage>>,

  pub(crate) targets: IdxVec<TargetId, Option<TargetDistillation>>,
  pub(crate) returns: Vec<Return>,
  pub(crate) closures: IdxVec<ClosureId, InterfaceId>,

  pub(crate) def: DefId,
  pub(crate) generics: GenericsId,
  pub(crate) types: Types<'core>,
  pub(crate) rels: Rels,
  pub(crate) locals: IdxVec<Local, TirLocal>,
}

#[derive(Debug)]
pub(crate) struct TargetDistillation {
  pub(crate) layer: LayerId,
  pub(crate) continue_transfer: Option<InterfaceId>,
  pub(crate) break_value: Local,
}

#[derive(Debug)]
pub(crate) struct Return {
  pub(crate) ty: Type,
  pub(crate) layer: LayerId,
  pub(crate) local: Local,
}

pub(crate) enum Poly<T = Port> {
  Error(ErrorGuaranteed),
  Value(T),
  Place((T, T)),
  Space(T),
}

impl<'core, 'r> Distiller<'core, 'r> {
  pub fn new(
    core: &'core Core<'core>,
    chart: &'r Chart<'core>,
    sigs: &'r Signatures<'core>,
  ) -> Self {
    Distiller {
      core,
      chart,
      sigs,
      layers: Default::default(),
      interfaces: Default::default(),
      stages: Default::default(),
      targets: Default::default(),
      returns: Default::default(),
      closures: Default::default(),
      locals: Default::default(),
      types: Types::default(),
      rels: Rels::default(),
      def: DefId(0),
      generics: GenericsId(0),
    }
  }

  pub fn distill_fragment(&mut self, fragment: &Fragment<'core>) -> Vir<'core> {
    self.def = fragment.def;
    self.generics = fragment.generics;
    self.locals = fragment.tir.locals.clone();
    self.types = fragment.tir.types.clone();
    self.rels = fragment.tir.rels.clone();
    let (layer, mut stage) = self.root_layer(fragment.tir.root.span);
    for (id, closure) in &fragment.tir.closures {
      let interface = self.distill_closure(closure);
      self.closures.push_to(id, interface);
    }
    let result = self.distill_expr_value(&mut stage, &fragment.tir.root);
    stage.header = Header::Entry(vec![result]);
    self.finish_stage(stage);
    self.finish_layer(layer);
    self.targets.clear();
    debug_assert!(self.returns.is_empty());
    let locals = IdxVec::from_iter(take(&mut self.locals).into_iter().map(|(_, local)| {
      let Self { core, chart, sigs, def, generics, ref mut types, ref mut rels, .. } = *self;
      VirLocal::new(core, chart, sigs, def, generics, types, rels, local.span, local.ty)
    }));
    Vir {
      types: take(&mut self.types),
      rels: take(&mut self.rels),
      layers: unwrap_idx_vec(take(&mut self.layers)),
      interfaces: unwrap_idx_vec(take(&mut self.interfaces)),
      stages: unwrap_idx_vec(take(&mut self.stages)),
      locals,
      closures: take(&mut self.closures),
    }
  }

  pub(crate) fn new_stage(
    &mut self,
    layer: &mut Layer,
    span: Span,
    interface: InterfaceId,
  ) -> Stage {
    let id = self.stages.push(None);
    layer.stages.push(id);
    Stage {
      id,
      span,
      layer: layer.id,
      interface,
      header: Header::None,
      declarations: Vec::new(),
      steps: Vec::new(),
      transfer: None,
      wires: Counter::default(),
    }
  }

  pub(crate) fn new_layer(&mut self) -> Layer {
    let id = self.layers.push(None);
    Layer { id, parent: None, stages: Vec::new() }
  }

  pub(crate) fn child_layer(&mut self, parent_stage: &mut Stage, span: Span) -> (Layer, Stage) {
    let mut layer = self.new_layer();
    layer.parent = Some(parent_stage.layer);
    let stage = self.new_unconditional_stage(&mut layer, span);
    parent_stage.steps.push(Step::Transfer(Transfer::unconditional(stage.interface)));
    (layer, stage)
  }

  fn root_layer(&mut self, span: Span) -> (Layer, Stage) {
    let mut layer = self.new_layer();
    let stage = self.new_unconditional_stage(&mut layer, span);
    (layer, stage)
  }

  pub(crate) fn finish_stage(&mut self, stage: Stage) -> StageId {
    let id = stage.id;
    self.stages[id] = Some(stage);
    id
  }

  pub(crate) fn finish_layer(&mut self, layer: Layer) {
    let id = layer.id;
    self.layers[id] = Some(layer);
  }

  pub(crate) fn new_unconditional_stage(&mut self, layer: &mut Layer, span: Span) -> Stage {
    let interface = self.interfaces.push(None);
    let stage = self.new_stage(layer, span, interface);
    self.interfaces[interface] =
      Some(Interface::new(interface, layer.id, InterfaceKind::Unconditional(stage.id)));
    stage
  }

  pub(crate) fn new_local(&mut self, stage: &mut Stage, span: Span, ty: Type) -> Local {
    let local = self.locals.push(TirLocal { span, ty });
    stage.declarations.push(local);
    local
  }

  pub(crate) fn distill_vec<T>(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    tuple: &[T],
    mut f: impl FnMut(&mut Self, &mut Stage, &T) -> Port,
    s: impl Fn(Port, Vec<Port>) -> Step,
  ) -> Port {
    let ports = tuple.iter().map(|x| f(self, stage, x)).collect::<Vec<_>>();
    let wire = stage.new_wire(span, ty);
    stage.steps.push(s(wire.neg, ports));
    wire.pos
  }

  pub(crate) fn distill_vec_pair<T>(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    tuple: &[T],
    mut f: impl FnMut(&mut Self, &mut Stage, &T) -> (Port, Port),
    s: impl Fn(Port, Vec<Port>) -> Step,
  ) -> (Port, Port) {
    let (lefts, rights) = tuple.iter().map(|x| f(self, stage, x)).collect::<(Vec<_>, Vec<_>)>();
    let left = stage.new_wire(span, ty);
    let right = stage.new_wire(span, ty);
    stage.steps.push(s(left.neg, lefts));
    stage.steps.push(s(right.pos, rights));
    (left.pos, right.neg)
  }

  pub(crate) fn distill_expr_nil(&mut self, stage: &mut Stage, expr: &TirExpr) {
    match &*expr.kind {
      TirExprKind![!nil] => self.distill_expr_nil_coerce_value(stage, expr),
      TirExprKind::Return(value) => self.distill_return(stage, value),
      TirExprKind::Break(label, value) => self.distill_break(stage, *label, value),
      TirExprKind::Continue(label) => self.distill_continue(stage, *label),
      TirExprKind::Assign(inverse, space, value) => {
        self.distill_expr_nil_assign(stage, *inverse, space, value)
      }
      TirExprKind::CallAssign(func, lhs, rhs) => {
        self.distill_expr_nil_call_assign(stage, *func, lhs, rhs)
      }
    }
  }

  pub(crate) fn distill_expr_value(&mut self, stage: &mut Stage, expr: &TirExpr) -> Port {
    let span = expr.span;
    let ty = expr.ty;
    match &*expr.kind {
      TirExprKind::Error(err) => Port::error(ty, *err),
      TirExprKind![nil] => self.distill_expr_value_coerce_nil(stage, expr, ty),
      TirExprKind![!value] => self.distill_expr_value_coerce_poly(stage, expr, span, ty),
      TirExprKind![cond] => self.distill_cond_bool(stage, span, ty, expr, false),
      TirExprKind::Closure(closure_id) => {
        self.distill_expr_value_closure(stage, span, ty, *closure_id)
      }
      TirExprKind::Seq(ignored, continuation) => self.distill_seq(stage, ignored, continuation),
      TirExprKind::Let(pat, init, continuation) => self.distill_let(stage, pat, init, continuation),
      TirExprKind::LetElse(pat, init, else_block, continuation) => {
        self.distill_let_else(stage, span, pat, init, else_block, continuation)
      }
      TirExprKind::Do(label, block) => self.distill_do(stage, span, *label, block),
      TirExprKind::If(arms, leg) => self.distill_if(stage, span, ty, arms, leg),
      TirExprKind::Loop(label, block) => self.distill_loop(stage, span, ty, *label, block),
      TirExprKind::While(label, cond, block, else_) => {
        self.distill_while(stage, span, ty, *label, cond, block, else_)
      }
      TirExprKind::For(label, rel, pat, iter, block, else_) => {
        self.distill_for(stage, span, ty, *label, *rel, pat, iter, block, else_)
      }
      TirExprKind::Match(value, arms) => self.distill_match(stage, span, ty, value, arms),
      TirExprKind::Try(ok, err, result) => self.distill_try(stage, span, ty, *ok, *err, result),
      TirExprKind::Local(local) => self.distill_expr_value_local(stage, span, ty, *local),
      TirExprKind::N32(n) => self.distill_expr_value_n32(ty, *n),
      TirExprKind::I32(i) => self.distill_expr_value_i32(ty, *i),
      TirExprKind::F32(f) => self.distill_expr_value_f32(ty, *f),
      TirExprKind::Char(c) => self.distill_expr_value_char(ty, *c),
      TirExprKind::Const(id) => self.distill_expr_value_const(ty, *id),
      TirExprKind::Fn => self.distill_expr_value_fn(ty),
      TirExprKind::Struct(struct_id, inner) => {
        self.distill_expr_value_struct(stage, span, ty, *struct_id, inner)
      }
      TirExprKind::Unwrap(struct_id, inner) => {
        self.distill_expr_value_unwrap(stage, span, ty, *struct_id, inner)
      }
      TirExprKind::Ref(place) => self.distill_expr_value_ref(stage, span, ty, place),
      TirExprKind::Inverse(inner) => self.distill_expr_value_inverse(stage, inner),
      TirExprKind::Composite(elements) => {
        self.distill_expr_value_composite(stage, span, ty, elements)
      }
      TirExprKind::Enum(enum_id, variant_id, data) => {
        self.distill_expr_value_enum(stage, span, ty, *enum_id, *variant_id, data)
      }
      TirExprKind::List(list) => self.distill_expr_value_list(stage, span, ty, list),
      TirExprKind::Call(rel, receiver, args) => {
        self.distill_expr_value_call(stage, span, ty, *rel, receiver, args)
      }
      TirExprKind::String(init, rest) => {
        self.distill_expr_value_string(stage, span, ty, init, rest)
      }
      TirExprKind::CallCompare(init, cmps) => {
        self.distill_expr_value_call_compare(stage, span, ty, init, cmps)
      }

      TirExprKind::InlineIvy(binds, net) => {
        self.distill_expr_value_inline_ivy(stage, span, ty, binds, net)
      }
    }
  }

  pub(crate) fn distill_expr_space(&mut self, stage: &mut Stage, expr: &TirExpr) -> Port {
    let span = expr.span;
    let ty = expr.ty;
    match &*expr.kind {
      TirExprKind::Error(err) => Port::error(ty.inverse(), *err),
      TirExprKind![!space] => self.distill_expr_space_coerce_poly(stage, expr, span, ty),
      TirExprKind::Local(local) => self.distill_expr_space_local(stage, span, ty, *local),
      TirExprKind::Struct(struct_id, inner) => {
        self.distill_expr_space_struct(stage, span, ty, *struct_id, inner)
      }
      TirExprKind::Unwrap(struct_id, inner) => {
        self.distill_expr_space_unwrap(stage, span, ty, *struct_id, inner)
      }
      TirExprKind::Hole => self.distill_expr_space_hole(stage, span, ty),
      TirExprKind::Inverse(inner) => self.distill_expr_space_inverse(stage, inner),
      TirExprKind::Composite(elements) => {
        self.distill_expr_space_composite(stage, span, ty, elements)
      }
    }
  }

  pub(crate) fn distill_expr_place(&mut self, stage: &mut Stage, expr: &TirExpr) -> (Port, Port) {
    let span = expr.span;
    let ty = expr.ty;
    match &*expr.kind {
      TirExprKind::Error(err) => (Port::error(ty, *err), Port::error(ty.inverse(), *err)),
      TirExprKind![!place] => self.distill_expr_place_coerce_poly(stage, expr, span, ty),
      TirExprKind::Struct(struct_id, inner) => {
        self.distill_expr_place_struct(stage, span, ty, *struct_id, inner)
      }
      TirExprKind::Unwrap(struct_id, inner) => {
        self.distill_expr_place_unwrap(stage, span, ty, *struct_id, inner)
      }
      TirExprKind::Local(local) => self.distill_expr_place_local(stage, span, ty, *local),
      TirExprKind::Deref(reference) => self.distill_expr_place_deref(stage, span, ty, reference),
      TirExprKind::Inverse(inner) => self.distill_expr_place_inverse(stage, inner),
      TirExprKind::Place(value, space) => self.distill_expr_place_place(stage, value, space),
      TirExprKind::Composite(elements) => {
        self.distill_expr_place_composite(stage, span, ty, elements)
      }
    }
  }

  pub(crate) fn distill_expr_poly(&mut self, stage: &mut Stage, expr: &TirExpr) -> Poly {
    let span = expr.span;
    let ty = expr.ty;
    match &*expr.kind {
      TirExprKind::Error(err) => Poly::Error(*err),
      TirExprKind![value && !place && !space] => Poly::Value(self.distill_expr_value(stage, expr)),
      TirExprKind![place && !value && !space] => Poly::Place(self.distill_expr_place(stage, expr)),
      TirExprKind![space && !place && !value] => Poly::Space(self.distill_expr_space(stage, expr)),
      TirExprKind::Struct(struct_id, inner) => {
        self.distill_expr_poly_struct(stage, span, ty, *struct_id, inner)
      }
      TirExprKind::Unwrap(struct_id, inner) => {
        self.distill_expr_poly_unwrap(stage, span, ty, *struct_id, inner)
      }
      TirExprKind::Local(_) => Poly::Place(self.distill_expr_place(stage, expr)),
      TirExprKind::Inverse(inner) => self.distill_expr_poly_inverse(stage, inner),
      TirExprKind::Composite(els) => self.distill_expr_poly_composite(stage, span, ty, els),
      TirExprKind::Field(inner, index, fields) => {
        self.distill_expr_poly_field(stage, span, ty, inner, *index, fields)
      }
    }
  }

  pub(crate) fn distill_pat_nil(&mut self, stage: &mut Stage, pat: &TirPat) {
    match &*pat.kind {
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
        self.distill_pat_nil_local(stage, *local);
      }
    }
  }

  pub(crate) fn distill_pat_value(&mut self, stage: &mut Stage, pat: &TirPat) -> Port {
    let span = pat.span;
    let ty = pat.ty;
    match &*pat.kind {
      TirPatKind::Error(err) => Port::error(ty.inverse(), *err),
      TirPatKind![!complete] => {
        Port::error(ty.inverse(), self.core.report(Diag::ExpectedCompletePat { span }))
      }
      TirPatKind::Deref(..) => {
        Port::error(ty.inverse(), self.core.report(Diag::DerefNonPlacePat { span }))
      }
      TirPatKind::Hole => self.distill_pat_value_hole(stage, span, ty),
      TirPatKind::Struct(struct_id, inner) => {
        self.distill_pat_value_struct(stage, span, ty, *struct_id, inner)
      }
      TirPatKind::Inverse(inner) => self.distill_pat_value_inverse(stage, inner),
      TirPatKind::Local(local) => self.distill_pat_value_local(stage, span, ty, *local),
      TirPatKind::Composite(elements) => {
        self.distill_pat_value_composite(stage, span, ty, elements)
      }
      TirPatKind::Ref(place) => self.distill_pat_value_ref(stage, span, ty, place),
    }
  }

  pub(crate) fn distill_pat_space(&mut self, stage: &mut Stage, pat: &TirPat) -> Port {
    let span = pat.span;
    let ty = pat.ty;
    match &*pat.kind {
      TirPatKind::Error(err) => Port::error(ty, *err),
      TirPatKind![!complete] => {
        Port::error(ty, self.core.report(Diag::ExpectedCompletePat { span }))
      }
      TirPatKind::Deref(..) => Port::error(ty, self.core.report(Diag::DerefNonPlacePat { span })),
      TirPatKind::Ref(..) => Port::error(ty, self.core.report(Diag::RefSpacePat { span })),
      TirPatKind::Hole => self.distill_pat_space_hole(stage, span, ty),
      TirPatKind::Struct(struct_id, inner) => {
        self.distill_pat_space_struct(stage, span, ty, *struct_id, inner)
      }
      TirPatKind::Inverse(inner) => self.distill_pat_space_inverse(stage, inner),
      TirPatKind::Local(local) => self.distill_pat_space_local(stage, span, ty, *local),
      TirPatKind::Composite(elements) => {
        self.distill_pat_space_composite(stage, span, ty, elements)
      }
    }
  }

  pub(crate) fn distill_pat_place(&mut self, stage: &mut Stage, pat: &TirPat) -> (Port, Port) {
    let span = pat.span;
    let ty = pat.ty;
    match &*pat.kind {
      TirPatKind::Error(err) => (Port::error(ty.inverse(), *err), Port::error(ty, *err)),
      TirPatKind![!complete] => {
        let err = self.core.report(Diag::ExpectedCompletePat { span });
        (Port::error(ty.inverse(), err), Port::error(ty, err))
      }
      TirPatKind::Hole => self.distill_pat_place_hole(stage, span, ty),
      TirPatKind::Struct(struct_id, inner) => {
        self.distill_pat_place_struct(stage, span, ty, *struct_id, inner)
      }
      TirPatKind::Inverse(inner) => self.distill_pat_place_inverse(stage, inner),
      TirPatKind::Local(local) => self.distill_pat_place_local(stage, span, ty, local),
      TirPatKind::Composite(elements) => {
        self.distill_pat_place_composite(stage, span, ty, elements)
      }
      TirPatKind::Ref(place) => self.distill_pat_place_ref(stage, span, ty, place),
      TirPatKind::Deref(reference) => self.distill_pat_place_deref(stage, span, ty, reference),
    }
  }
}
