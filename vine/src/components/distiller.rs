use std::mem::{replace, take};

use vine_util::{
  idx::{Counter, IdxVec},
  unwrap_idx_vec,
};

use crate::{
  components::{analyzer::usage::Usage, finder::Finder},
  structures::{
    ast::Span,
    chart::{Chart, DefId, FnId, GenericsId, TraitFnId},
    core::Core,
    diag::{Diag, ErrorGuaranteed},
    resolutions::{FnRel, Fragment, Rels},
    signatures::Signatures,
    tir::{ClosureId, LabelId, Local, TirExpr, TirExprKind, TirPat, TirPatKind},
    types::{ImplType, Type, TypeKind, Types},
    vir::{
      Header, Interface, InterfaceId, InterfaceKind, Layer, LayerId, Port, PortKind, Stage,
      StageId, Step, Transfer, Vir,
    },
  },
};

mod control_flow;
mod pattern_matching;

#[derive(Debug)]
pub struct Distiller<'core, 'r> {
  core: &'core Core<'core>,
  chart: &'r Chart<'core>,
  sigs: &'r Signatures<'core>,

  layers: IdxVec<LayerId, Option<Layer>>,
  interfaces: IdxVec<InterfaceId, Option<Interface>>,
  stages: IdxVec<StageId, Option<Stage>>,

  labels: IdxVec<LabelId, Option<Label>>,
  returns: Vec<Return>,
  closures: IdxVec<ClosureId, InterfaceId>,

  def: DefId,
  generics: GenericsId,
  types: Types<'core>,
  rels: Rels,
  locals: IdxVec<Local, Type>,
}

#[derive(Debug)]
struct Label {
  layer: LayerId,
  continue_transfer: Option<InterfaceId>,
  break_value: Option<Local>,
}

#[derive(Debug)]
struct Return {
  ty: Type,
  layer: LayerId,
  local: Local,
}

enum Poly<T = Port> {
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
      labels: Default::default(),
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
    let (layer, mut stage) = self.root_layer();
    let local = self.locals.push(fragment.tir.root.ty);
    for closure in fragment.tir.closures.values() {
      let interface = self.distill_closure_def(closure);
      self.closures.push(interface);
    }
    let result = self.distill_expr_value(&mut stage, &fragment.tir.root);
    stage.set_local_to(local, result);
    self.finish_stage(stage);
    self.finish_layer(layer);
    self.labels.clear();
    debug_assert!(self.returns.is_empty());
    Vir {
      types: take(&mut self.types),
      rels: take(&mut self.rels),
      layers: unwrap_idx_vec(take(&mut self.layers)),
      interfaces: unwrap_idx_vec(take(&mut self.interfaces)),
      stages: unwrap_idx_vec(take(&mut self.stages)),
      locals: take(&mut self.locals),
      globals: vec![(local, Usage::Take)],
      closures: take(&mut self.closures),
    }
  }

  fn distill_expr_nil(&mut self, stage: &mut Stage, expr: &TirExpr) {
    match &*expr.kind {
      TirExprKind![!nil] => {
        let value = self.distill_expr_value(stage, expr);
        self.drop(expr.span, stage, expr.ty, value);
      }
      TirExprKind::While(label, cond, block) => self.distill_while(stage, *label, cond, block),
      TirExprKind::Return(value) => self.distill_return(stage, value),
      TirExprKind::Break(label, value) => self.distill_break(stage, *label, value),
      TirExprKind::Continue(label) => self.distill_continue(stage, *label),
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
      }
      TirExprKind::CallAssign(func, lhs, rhs) => {
        let rhs = self.distill_expr_value(stage, rhs);
        let (lhs, out) = self.distill_expr_place(stage, lhs);
        stage.steps.push(Step::Call(*func, None, vec![lhs, rhs], out));
      }
    }
  }

  fn distill_expr_value(&mut self, stage: &mut Stage, expr: &TirExpr) -> Port {
    let span = expr.span;
    let ty = expr.ty;
    match &*expr.kind {
      TirExprKind::Error(err) => Port::error(ty, *err),
      TirExprKind![nil] => {
        self.distill_expr_nil(stage, expr);
        Port { ty, kind: PortKind::Nil }
      }
      TirExprKind![!value] => match self.distill_expr_poly(stage, expr) {
        Poly::Error(err) => Port::error(ty, err),
        Poly::Value(value) => value,
        Poly::Place((value, space)) => {
          let value = stage.dup(value);
          stage.steps.push(Step::Link(value.0, space));
          value.1
        }
        Poly::Space(_) => {
          Port::error(ty, self.core.report(Diag::ExpectedValueFoundSpaceExpr { span }))
        }
      },

      TirExprKind![cond] => self.distill_cond_bool(stage, ty, expr, false),
      TirExprKind::Closure(closure_id) => self.distill_closure(stage, ty, *closure_id),
      TirExprKind::Seq(ignored, continuation) => self.distill_seq(stage, ignored, continuation),
      TirExprKind::Let(pat, init, continuation) => self.distill_let(stage, pat, init, continuation),
      TirExprKind::LetElse(pat, init, else_block, continuation) => {
        self.distill_let_else(stage, pat, init, else_block, continuation)
      }
      TirExprKind::Do(label, block) => self.distill_do(stage, *label, block),
      TirExprKind::If(arms, leg) => self.distill_if(stage, ty, arms, leg),
      TirExprKind::Loop(label, block) => self.distill_loop(stage, ty, *label, block),
      TirExprKind::Match(value, arms) => self.distill_match(span, stage, ty, value, arms),
      TirExprKind::Try(ok, err, result) => self.distill_try(stage, ty, *ok, *err, result),

      TirExprKind::Local(local) => stage.get_local(*local, ty),

      TirExprKind::Char(c) => Port { ty, kind: PortKind::N32(*c as u32) },
      TirExprKind::N32(n) => Port { ty, kind: PortKind::N32(*n) },
      TirExprKind::I32(n) => Port { ty, kind: PortKind::N32(*n as u32) },
      TirExprKind::F32(f) => Port { ty, kind: PortKind::F32(*f) },

      TirExprKind::Const(id) => Port { ty, kind: PortKind::ConstRel(*id) },
      TirExprKind::Fn => Port { ty, kind: PortKind::Nil },

      TirExprKind::Unwrap(struct_id, inner) => {
        let inner = self.distill_expr_value(stage, inner);
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, inner, wire.neg));
        wire.pos
      }
      TirExprKind::Struct(struct_id, inner) => {
        let inner = self.distill_expr_value(stage, inner);
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, wire.neg, inner));
        wire.pos
      }

      TirExprKind::Ref(place) => {
        let place = self.distill_expr_place(stage, place);
        stage.ref_place(ty, place)
      }
      TirExprKind::Inverse(inner) => self.distill_expr_space(stage, inner),
      TirExprKind::Composite(elements) => {
        self.distill_vec(stage, ty, elements, Self::distill_expr_value, Step::Composite)
      }
      TirExprKind::Enum(enum_id, variant_id, data) => {
        let data = data.as_ref().map(|e| self.distill_expr_value(stage, e));
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Enum(*enum_id, *variant_id, wire.neg, data));
        wire.pos
      }
      TirExprKind::List(list) => {
        self.distill_vec(stage, ty, list, Self::distill_expr_value, Step::List)
      }
      TirExprKind::Call(rel, receiver, args) => {
        let receiver = receiver.as_ref().map(|r| self.distill_expr_value(stage, r));
        let args = args.iter().map(|s| self.distill_expr_value(stage, s)).collect::<Vec<_>>();
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Call(*rel, receiver, args, wire.neg));
        wire.pos
      }
      TirExprKind::String(init, rest) => {
        let wire = stage.new_wire(ty);
        let rest = rest
          .iter()
          .map(|(expr, seg)| (self.distill_expr_value(stage, expr), seg.clone()))
          .collect();
        stage.steps.push(Step::String(wire.neg, init.clone(), rest));
        wire.pos
      }
      TirExprKind::CallCompare(init, cmps) => {
        let mut last_result = Port { ty, kind: PortKind::Nil };
        let lhs = self.distill_expr_place(stage, init);
        let ref_ty = self.types.new(TypeKind::Ref(init.ty));
        let mut lhs = Some(stage.ref_place(ref_ty, lhs));
        for (i, (func, rhs)) in cmps.iter().enumerate() {
          let first = i == 0;
          let last = i == cmps.len() - 1;
          let rhs = self.distill_expr_place(stage, rhs);
          let (rhs, next_lhs) = if last {
            (stage.ref_place(ref_ty, rhs), None)
          } else {
            let wire = stage.new_wire(init.ty);
            (
              stage.ref_place(ref_ty, (rhs.0, wire.neg)),
              Some(stage.ref_place(ref_ty, (wire.pos, rhs.1))),
            )
          };
          let result = stage.new_wire(ty);
          stage.steps.push(Step::Call(*func, None, vec![lhs.unwrap(), rhs], result.neg));
          last_result = if first {
            result.pos
          } else {
            stage.ext_fn("n32_and", false, last_result, result.pos, ty)
          };
          lhs = next_lhs;
        }
        last_result
      }

      TirExprKind::InlineIvy(binds, net) => {
        let out = stage.new_wire(ty);
        let binds = binds
          .iter()
          .map(|(var, value, expr)| {
            (
              var.clone(),
              if *value {
                self.distill_expr_value(stage, expr)
              } else {
                self.distill_expr_space(stage, expr)
              },
            )
          })
          .collect();
        stage.steps.push(Step::InlineIvy(binds, out.neg, net.clone()));
        out.pos
      }
    }
  }

  fn distill_expr_space(&mut self, stage: &mut Stage, expr: &TirExpr) -> Port {
    let span = expr.span;
    let ty = expr.ty;
    let tyi = ty.inverse();
    match &*expr.kind {
      TirExprKind::Error(err) => Port::error(tyi, *err),
      TirExprKind![!space] => match self.distill_expr_poly(stage, expr) {
        Poly::Error(err) => Port::error(tyi, err),
        Poly::Value(_) => {
          Port::error(tyi, self.core.report(Diag::ExpectedSpaceFoundValueExpr { span }))
        }
        Poly::Place((value, space)) => {
          self.drop(span, stage, expr.ty, value);
          space
        }
        Poly::Space(space) => space,
      },
      TirExprKind::Local(local) => stage.set_local(*local, ty),
      TirExprKind::Unwrap(struct_id, inner) => {
        let inner = self.distill_expr_space(stage, inner);
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, inner, wire.pos));
        wire.neg
      }
      TirExprKind::Struct(struct_id, inner) => {
        let inner = self.distill_expr_space(stage, inner);
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, wire.pos, inner));
        wire.neg
      }
      TirExprKind::Hole => self.drop_space(span, stage, ty),
      TirExprKind::Inverse(inner) => self.distill_expr_value(stage, inner),
      TirExprKind::Composite(elements) => {
        self.distill_vec(stage, tyi, elements, Self::distill_expr_space, Step::Composite)
      }
    }
  }

  fn distill_expr_place(&mut self, stage: &mut Stage, expr: &TirExpr) -> (Port, Port) {
    let span = expr.span;
    let ty = expr.ty;
    let tyi = ty.inverse();
    match &*expr.kind {
      TirExprKind::Error(err) => (Port::error(ty, *err), Port::error(tyi, *err)),
      TirExprKind![!place] => match self.distill_expr_poly(stage, expr) {
        Poly::Error(err) => (Port::error(ty, err), Port::error(tyi, err)),
        Poly::Value(value) => (value, self.drop_space(span, stage, ty)),
        Poly::Place(place) => place,
        Poly::Space(space) => (self.drop_space(span, stage, ty.inverse()), space),
      },
      TirExprKind::Unwrap(struct_id, inner) => {
        let (inner_value, inner_space) = self.distill_expr_place(stage, inner);
        let value = stage.new_wire(ty);
        let space = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, inner_value, value.neg));
        stage.steps.push(Step::Struct(*struct_id, inner_space, space.pos));
        (value.pos, space.neg)
      }
      TirExprKind::Struct(struct_id, inner) => {
        let (inner_value, inner_space) = self.distill_expr_place(stage, inner);
        let value = stage.new_wire(ty);
        let space = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, value.neg, inner_value));
        stage.steps.push(Step::Struct(*struct_id, space.pos, inner_space));
        (value.pos, space.neg)
      }
      TirExprKind::Local(local) => stage.mut_local(*local, ty),
      TirExprKind::Deref(reference) => {
        let reference = self.distill_expr_value(stage, reference);
        let value = stage.new_wire(ty);
        let space = stage.new_wire(ty);
        stage.steps.push(Step::Ref(reference, value.neg, space.pos));
        (value.pos, space.neg)
      }
      TirExprKind::Inverse(inner) => {
        let (value, space) = self.distill_expr_place(stage, inner);
        (space, value)
      }
      TirExprKind::Place(value, space) => {
        (self.distill_expr_value(stage, value), self.distill_expr_space(stage, space))
      }
      TirExprKind::Composite(elements) => {
        self.distill_vec_pair(stage, ty, elements, Self::distill_expr_place, Step::Composite)
      }
    }
  }

  fn distill_expr_poly(&mut self, stage: &mut Stage, expr: &TirExpr) -> Poly {
    let span = expr.span;
    let ty = expr.ty;
    match &*expr.kind {
      TirExprKind::Error(err) => Poly::Error(*err),
      TirExprKind![(value || nil) && !place && !space] => {
        Poly::Value(self.distill_expr_value(stage, expr))
      }
      TirExprKind![place && !value && !space] => Poly::Place(self.distill_expr_place(stage, expr)),
      TirExprKind![space && !place && !value] => Poly::Space(self.distill_expr_space(stage, expr)),
      TirExprKind::Unwrap(struct_id, inner) => match self.distill_expr_poly(stage, inner) {
        Poly::Error(err) => Poly::Error(err),
        Poly::Value(inner_value) => {
          let value = stage.new_wire(ty);
          stage.steps.push(Step::Struct(*struct_id, inner_value, value.neg));
          Poly::Value(value.pos)
        }
        Poly::Place((inner_value, inner_space)) => {
          let value = stage.new_wire(ty);
          let space = stage.new_wire(ty);
          stage.steps.push(Step::Struct(*struct_id, inner_value, value.neg));
          stage.steps.push(Step::Struct(*struct_id, inner_space, space.pos));
          Poly::Place((value.pos, space.neg))
        }
        Poly::Space(inner_space) => {
          let space = stage.new_wire(ty);
          stage.steps.push(Step::Struct(*struct_id, inner_space, space.pos));
          Poly::Space(space.neg)
        }
      },
      TirExprKind::Struct(struct_id, inner) => match self.distill_expr_poly(stage, inner) {
        Poly::Error(err) => Poly::Error(err),
        Poly::Value(inner_value) => {
          let value = stage.new_wire(ty);
          stage.steps.push(Step::Struct(*struct_id, value.neg, inner_value));
          Poly::Value(value.pos)
        }
        Poly::Place((inner_value, inner_space)) => {
          let value = stage.new_wire(ty);
          let space = stage.new_wire(ty);
          stage.steps.push(Step::Struct(*struct_id, value.neg, inner_value));
          stage.steps.push(Step::Struct(*struct_id, space.pos, inner_space));
          Poly::Place((value.pos, space.neg))
        }
        Poly::Space(inner_space) => {
          let space = stage.new_wire(ty);
          stage.steps.push(Step::Struct(*struct_id, space.pos, inner_space));
          Poly::Space(space.neg)
        }
      },
      TirExprKind::Local(_) => Poly::Place(self.distill_expr_place(stage, expr)),
      TirExprKind::Inverse(inner) => match self.distill_expr_poly(stage, inner) {
        Poly::Error(err) => Poly::Error(err),
        Poly::Value(p) => Poly::Space(p),
        Poly::Place((p, q)) => Poly::Place((q, p)),
        Poly::Space(p) => Poly::Value(p),
      },
      TirExprKind::Composite(els) => {
        let mut acc = None;
        for el in els {
          let el = self.distill_expr_poly(stage, el);
          acc = Some({
            let mut acc = acc.unwrap_or_else(|| match el {
              Poly::Error(err) => Poly::Error(err),
              Poly::Value(_) => Poly::Value(vec![]),
              Poly::Place(_) => Poly::Place((vec![], vec![])),
              Poly::Space(_) => Poly::Space(vec![]),
            });
            match (&mut acc, el) {
              (_, Poly::Error(err)) => acc = Poly::Error(err),
              (Poly::Value(ps), Poly::Value(p)) | (Poly::Space(ps), Poly::Space(p)) => ps.push(p),
              (Poly::Place((ps, qs)), Poly::Place((p, q))) => {
                ps.push(p);
                qs.push(q);
              }
              _ => acc = Poly::Error(self.core.report(Diag::AmbiguousPolyformicComposite { span })),
            }
            acc
          });
        }
        match acc.unwrap_or(Poly::Place((vec![], vec![]))) {
          Poly::Error(err) => Poly::Error(err),
          Poly::Value(values) => {
            let value = stage.new_wire(ty);
            stage.steps.push(Step::Composite(value.neg, values));
            Poly::Value(value.pos)
          }
          Poly::Place((values, spaces)) => {
            let value = stage.new_wire(ty);
            let space = stage.new_wire(ty);
            stage.steps.push(Step::Composite(value.neg, values));
            stage.steps.push(Step::Composite(space.pos, spaces));
            Poly::Place((value.pos, space.neg))
          }
          Poly::Space(spaces) => {
            let space = stage.new_wire(ty);
            stage.steps.push(Step::Composite(space.pos, spaces));
            Poly::Space(space.neg)
          }
        }
      }
      TirExprKind::Field(inner, index, fields) => {
        let inner = self.distill_expr_poly(stage, inner);
        match inner {
          Poly::Error(err) => Poly::Error(err),
          Poly::Value(value) => {
            let wire = stage.new_wire(ty);
            let mut neg = Some(wire.neg);
            let ports = Vec::from_iter(fields.iter().enumerate().map(|(i, &ty)| {
              if i == *index {
                neg.take().unwrap()
              } else {
                self.drop_space(span, stage, ty)
              }
            }));
            stage.steps.push(Step::Composite(value, ports));
            Poly::Value(wire.pos)
          }
          Poly::Place(place) => {
            let (mut neg, pos) = fields
              .iter()
              .map(|&ty| {
                let wire = stage.new_wire(ty);
                (wire.neg, wire.pos)
              })
              .collect::<(Vec<_>, Vec<_>)>();
            let wire = stage.new_wire(ty);
            let value = wire.pos;
            let space = replace(&mut neg[*index], wire.neg);
            stage.steps.push(Step::Composite(place.0, neg));
            stage.steps.push(Step::Composite(place.1, pos));
            Poly::Place((value, space))
          }
          Poly::Space(_) => Poly::Error(self.core.report(Diag::SpaceField { span })),
        }
      }
    }
  }

  #[allow(clippy::only_used_in_recursion)]
  fn distill_pat_nil(&mut self, stage: &mut Stage, pat: &TirPat) {
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
        stage.declarations.push(*local);
        stage.erase_local(*local);
      }
    }
  }

  fn distill_pat_value(&mut self, stage: &mut Stage, pat: &TirPat) -> Port {
    let span = pat.span;
    let ty = pat.ty;
    let tyi = pat.ty.inverse();
    match &*pat.kind {
      TirPatKind::Error(err) => Port::error(tyi, *err),
      TirPatKind![!complete] => {
        Port::error(tyi, self.core.report(Diag::ExpectedCompletePat { span }))
      }
      TirPatKind::Deref(..) => Port::error(tyi, self.core.report(Diag::DerefNonPlacePat { span })),
      TirPatKind::Hole => self.drop_space(span, stage, ty),
      TirPatKind::Struct(struct_id, inner) => {
        let inner = self.distill_pat_value(stage, inner);
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, wire.pos, inner));
        wire.neg
      }
      TirPatKind::Inverse(inner) => self.distill_pat_space(stage, inner),
      TirPatKind::Local(local) => {
        stage.declarations.push(*local);
        stage.set_local(*local, ty)
      }
      TirPatKind::Composite(elements) => {
        self.distill_vec(stage, tyi, elements, Self::distill_pat_value, Step::Composite)
      }
      TirPatKind::Ref(place) => {
        let (value, space) = self.distill_pat_place(stage, place);
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Ref(wire.pos, value, space));
        wire.neg
      }
    }
  }

  fn distill_pat_space(&mut self, stage: &mut Stage, pat: &TirPat) -> Port {
    let span = pat.span;
    let ty = pat.ty;
    let tyi = pat.ty.inverse();
    match &*pat.kind {
      TirPatKind::Error(err) => Port::error(ty, *err),
      TirPatKind![!complete] => {
        Port::error(ty, self.core.report(Diag::ExpectedCompletePat { span }))
      }
      TirPatKind::Deref(..) => Port::error(ty, self.core.report(Diag::DerefNonPlacePat { span })),
      TirPatKind::Ref(..) => Port::error(ty, self.core.report(Diag::RefSpacePat { span })),
      TirPatKind::Hole => self.drop_space(span, stage, tyi),
      TirPatKind::Struct(struct_id, inner) => {
        let inner = self.distill_pat_space(stage, inner);
        let wire = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, wire.neg, inner));
        wire.pos
      }
      TirPatKind::Inverse(inner) => self.distill_pat_value(stage, inner),
      TirPatKind::Local(local) => {
        stage.declarations.push(*local);
        stage.take_local(*local, ty)
      }
      TirPatKind::Composite(elements) => {
        self.distill_vec(stage, ty, elements, Self::distill_pat_space, Step::Composite)
      }
    }
  }

  fn distill_pat_place(&mut self, stage: &mut Stage, pat: &TirPat) -> (Port, Port) {
    let span = pat.span;
    let ty = pat.ty;
    let tyi = pat.ty.inverse();
    match &*pat.kind {
      TirPatKind::Error(err) => (Port::error(tyi, *err), Port::error(ty, *err)),
      TirPatKind![!complete] => {
        let err = self.core.report(Diag::ExpectedCompletePat { span });
        (Port::error(tyi, err), Port::error(ty, err))
      }
      TirPatKind::Hole => {
        let wire = stage.new_wire(ty);
        (wire.neg, wire.pos)
      }
      TirPatKind::Struct(struct_id, inner) => {
        let (inner_value, inner_space) = self.distill_pat_place(stage, inner);
        let value = stage.new_wire(ty);
        let space = stage.new_wire(ty);
        stage.steps.push(Step::Struct(*struct_id, value.pos, inner_value));
        stage.steps.push(Step::Struct(*struct_id, space.neg, inner_space));
        (value.neg, space.pos)
      }
      TirPatKind::Inverse(inner) => {
        let (value, space) = self.distill_pat_place(stage, inner);
        (space, value)
      }
      TirPatKind::Local(local) => {
        stage.declarations.push(*local);
        let (a, b) = stage.mut_local(*local, ty);
        (b, a)
      }
      TirPatKind::Composite(elements) => {
        self.distill_vec_pair(stage, tyi, elements, Self::distill_pat_place, Step::Composite)
      }
      TirPatKind::Ref(place) => {
        let (value_0, value_1) = self.distill_pat_place(stage, place);
        let ref_in = stage.new_wire(ty);
        let ref_out = stage.new_wire(ty);
        let value_2 = stage.new_wire(place.ty);
        stage.steps.push(Step::Ref(ref_in.pos, value_0, value_2.pos));
        stage.steps.push(Step::Ref(ref_out.neg, value_1, value_2.neg));
        (ref_in.neg, ref_out.pos)
      }
      TirPatKind::Deref(reference) => {
        let reference = self.distill_pat_value(stage, reference);
        let value = stage.new_wire(ty);
        let space = stage.new_wire(ty);
        stage.steps.push(Step::Ref(reference, value.pos, space.neg));
        (value.neg, space.pos)
      }
    }
  }

  fn drop(&mut self, span: Span, stage: &mut Stage, ty: Type, port: Port) {
    let Some(drop) = self.chart.builtins.drop else {
      self.core.report(Diag::MissingBuiltin { span, builtin: "Drop" });
      return;
    };
    let mut finder = Finder::new(self.core, self.chart, self.sigs, self.def, self.generics, span);
    let impl_ = finder.find_impl(&mut self.types, &ImplType::Trait(drop, vec![ty]));
    let fn_rel = self.rels.fns.push(FnRel::Item(FnId::Abstract(drop, TraitFnId(0)), vec![impl_]));
    let nil = Port { ty: self.types.nil(), kind: PortKind::Nil };
    stage.steps.push(Step::Call(fn_rel, None, vec![port], nil));
  }

  fn drop_space(&mut self, span: Span, stage: &mut Stage, ty: Type) -> Port {
    let wire = stage.new_wire(ty);
    self.drop(span, stage, ty, wire.pos);
    wire.neg
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

  fn new_local(&mut self, stage: &mut Stage, ty: Type) -> Local {
    let local = self.locals.push(ty);
    stage.declarations.push(local);
    local
  }

  fn distill_vec<T>(
    &mut self,
    stage: &mut Stage,
    ty: Type,
    tuple: &[T],
    mut f: impl FnMut(&mut Self, &mut Stage, &T) -> Port,
    s: impl Fn(Port, Vec<Port>) -> Step,
  ) -> Port {
    let ports = tuple.iter().map(|x| f(self, stage, x)).collect::<Vec<_>>();
    let wire = stage.new_wire(ty);
    stage.steps.push(s(wire.neg, ports));
    wire.pos
  }

  fn distill_vec_pair<T>(
    &mut self,
    stage: &mut Stage,
    ty: Type,
    tuple: &[T],
    mut f: impl FnMut(&mut Self, &mut Stage, &T) -> (Port, Port),
    s: impl Fn(Port, Vec<Port>) -> Step,
  ) -> (Port, Port) {
    let (lefts, rights) = tuple.iter().map(|x| f(self, stage, x)).collect::<(Vec<_>, Vec<_>)>();
    let left = stage.new_wire(ty);
    let right = stage.new_wire(ty);
    stage.steps.push(s(left.neg, lefts));
    stage.steps.push(s(right.pos, rights));
    (left.pos, right.neg)
  }
}
