use crate::{
  components::{
    distiller::{Distiller, Poly},
    finder::Finder,
  },
  structures::{
    ast::Span,
    diag::Diag,
    tir::TirExpr,
    types::{ImplType, Inverted, Type, TypeKind},
    vir::{Port, PortKind, Stage, Step},
  },
};

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_coerce_poly(
    &mut self,
    stage: &mut Stage,
    expr: &TirExpr,
    span: Span,
    ty: Type,
  ) -> Port {
    match self.distill_expr_poly(stage, expr) {
      Poly::Error(err) => Port::error(ty, err),
      Poly::Value(value) => value,
      Poly::Place(place) => self.coerce_place_value(span, stage, place),
      Poly::Space(_) => {
        Port::error(ty, self.diags.error(Diag::ExpectedValueFoundSpaceExpr { span }))
      }
    }
  }

  pub(crate) fn distill_expr_space_coerce_poly(
    &mut self,
    stage: &mut Stage,
    expr: &TirExpr,
    span: Span,
    ty: Type,
  ) -> Port {
    match self.distill_expr_poly(stage, expr) {
      Poly::Error(err) => Port::error(ty.inverse(), err),
      Poly::Value(_) => {
        Port::error(ty.inverse(), self.diags.error(Diag::ExpectedSpaceFoundValueExpr { span }))
      }
      Poly::Place(place) => self.coerce_place_space(span, stage, place),
      Poly::Space(space) => space,
    }
  }

  pub(crate) fn distill_expr_place_coerce_poly(
    &mut self,
    stage: &mut Stage,
    expr: &TirExpr,
    span: Span,
    ty: Type,
  ) -> (Port, Port) {
    match self.distill_expr_poly(stage, expr) {
      Poly::Error(err) => (Port::error(ty, err), Port::error(ty, err)),
      Poly::Value(value) => (value, self.drop_space(span, stage, ty)),
      Poly::Place(place) => place,
      Poly::Space(space) => (self.drop_space(span, stage, ty.inverse()), space),
    }
  }

  pub(crate) fn distill_expr_nil_coerce_value(&mut self, stage: &mut Stage, expr: &TirExpr) {
    let value = self.distill_expr_value(stage, expr);
    self.drop(expr.span, stage, expr.ty, value);
  }

  pub(crate) fn distill_expr_value_coerce_nil(
    &mut self,
    stage: &mut Stage,
    expr: &TirExpr,
    ty: Type,
  ) -> Port {
    self.distill_expr_nil(stage, expr);
    Port { ty, kind: PortKind::Nil }
  }

  fn coerce_place_value(&mut self, span: Span, stage: &mut Stage, place: (Port, Port)) -> Port {
    self._coerce_place(span, stage, place, Inverted(false))
  }

  fn coerce_place_space(&mut self, span: Span, stage: &mut Stage, place: (Port, Port)) -> Port {
    self._coerce_place(span, stage, place, Inverted(true))
  }

  fn _coerce_place(
    &mut self,
    span: Span,
    stage: &mut Stage,
    (value, space): (Port, Port),
    inv: Inverted,
  ) -> Port {
    let ty = value.ty.invert_if(inv);
    let mut finder = Finder::new(
      self.chart,
      self.sigs,
      self.diags,
      self.finder_cache,
      self.def,
      self.generics,
      span,
    );
    let flex = match finder.find_flex(&mut self.types, value.ty) {
      Ok(flex) => flex,
      Err(err) => return Port::error(ty, err),
    };
    let (value, space) = if inv.0 { (space, value) } else { (value, space) };
    let inv = flex.inv ^ inv;
    match inv {
      Inverted(false) => match flex.fork {
        Some(impl_) => {
          let fork_rel = self.rels.fork_rel(self.chart, impl_);
          let ref_ty = self.types.new(TypeKind::Ref(ty));
          let ref_ = stage.new_wire(span, ref_ty);
          stage.steps.push(Step::Ref(ref_.neg, value, space));
          let out = stage.new_wire(span, ty);
          stage.steps.push(Step::Call(span, fork_rel, None, vec![ref_.pos], out.neg));
          out.pos
        }
        None => {
          let diag = Diag::CannotFork { span, ty: self.types.show(self.chart, ty) };
          Port::error(ty, self.diags.error(diag))
        }
      },
      Inverted(true) => match flex.drop {
        Some(impl_) => {
          let drop_rel = self.rels.drop_rel(self.chart, impl_);
          let nil = Port { ty: self.types.nil(), kind: PortKind::Nil };
          stage.steps.push(Step::Call(span, drop_rel, None, vec![space], nil));
          value
        }
        None => {
          let diag = Diag::CannotDrop { span, ty: self.types.show(self.chart, ty.inverse()) };
          Port::error(ty, self.diags.error(diag))
        }
      },
    }
  }

  pub(crate) fn drop(&mut self, span: Span, stage: &mut Stage, ty: Type, port: Port) {
    let Some(drop) = self.chart.builtins.drop else {
      self.diags.error(Diag::MissingBuiltin { span, builtin: "Drop" });
      return;
    };
    let impl_ = self.find_impl(span, &ImplType::Trait(drop, vec![ty]), false);
    let fn_rel = self.rels.drop_rel(self.chart, impl_);
    let nil = Port { ty: self.types.nil(), kind: PortKind::Nil };
    stage.steps.push(Step::Call(span, fn_rel, None, vec![port], nil));
  }

  pub(crate) fn drop_space(&mut self, span: Span, stage: &mut Stage, ty: Type) -> Port {
    let wire = stage.new_wire(span, ty);
    self.drop(span, stage, ty, wire.pos);
    wire.neg
  }
}
