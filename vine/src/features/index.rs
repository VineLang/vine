use crate::{
  components::{
    distiller::{Distiller, Poly},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, Span},
    chart::{FnId, TraitFnId},
    diag::Diag,
    resolutions::FnRel,
    tir::{TirExpr, TirExprKind},
    types::{ImplType, Type},
    vir::{Port, PortKind, Stage, Step},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_index(&self, expr: &Expr, index: &Expr) -> Doc<'src> {
    Doc::concat([self.fmt_expr(expr), Doc(".["), self.fmt_expr(index), Doc("]")])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_index(
    &mut self,
    span: Span,
    expr: &Expr,
    index: &Expr,
  ) -> Result<TirExpr, Diag> {
    let Some(index_trait) = self.chart.builtins.index else {
      Err(Diag::MissingBuiltin { span, builtin: "Index" })?
    };

    let expr = self.resolve_expr(expr);
    let index = self.resolve_expr(index);
    let value_ty = self.types.new_var(span);
    let impl_type = ImplType::Trait(index_trait, vec![expr.ty, index.ty, value_ty]);
    let _ = self.find_impl(span, &impl_type, false);

    Ok(TirExpr::new(span, value_ty, TirExprKind::Index(expr, index)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_index(
    &mut self,
    stage: &mut Stage,
    span: Span,
    expr: &TirExpr,
    index: &TirExpr,
    value_ty: Type,
  ) -> Port {
    let Some(index_value_trait) = self.chart.builtins.index_value else {
      return Port::error(
        value_ty,
        self.diags.error(Diag::MissingBuiltin { span, builtin: "IndexValue" }),
      );
    };

    let (expr_pos, expr_neg) = self.distill_expr_place(stage, expr);
    let expr_ref = stage.ref_place(span, expr.ty, (expr_pos, expr_neg));
    let impl_type = ImplType::Trait(index_value_trait, vec![expr.ty, index.ty, value_ty]);
    let index_value_impl = self.find_impl(span, &impl_type, false);
    let assume_get_rel =
      FnRel::Item(FnId::Abstract(index_value_trait, TraitFnId(0)), vec![index_value_impl]);
    let assume_get_rel_id = self.rels.fns.push(assume_get_rel);
    let args = vec![expr_ref, self.distill_expr_value(stage, index)];
    let value = stage.new_wire(span, value_ty);
    stage.steps.push(Step::Call(span, assume_get_rel_id, None, args, value.neg));
    value.pos
  }

  pub(crate) fn distill_expr_space_index(
    &mut self,
    stage: &mut Stage,
    span: Span,
    expr: &TirExpr,
    index: &TirExpr,
    value_ty: Type,
  ) -> Port {
    let Some(index_space_trait) = self.chart.builtins.index_space else {
      return Port::error(
        value_ty,
        self.diags.error(Diag::MissingBuiltin { span, builtin: "IndexSpace" }),
      );
    };

    let (expr_pos, expr_neg) = self.distill_expr_place(stage, expr);
    let expr_ref = stage.ref_place(span, expr.ty, (expr_pos, expr_neg));
    let impl_type = ImplType::Trait(index_space_trait, vec![expr.ty, index.ty, value_ty]);
    let index_space_impl = self.find_impl(span, &impl_type, false);
    let assume_set_rel =
      FnRel::Item(FnId::Abstract(index_space_trait, TraitFnId(0)), vec![index_space_impl]);
    let assume_set_rel_id = self.rels.fns.push(assume_set_rel);
    let space = stage.new_wire(span, value_ty);
    let args = vec![expr_ref, self.distill_expr_value(stage, index), space.pos];
    let nil = Port { ty: self.types.nil(), kind: PortKind::Nil };
    stage.steps.push(Step::Call(span, assume_set_rel_id, None, args, nil));
    space.neg
  }

  pub(crate) fn distill_expr_place_index(
    &mut self,
    stage: &mut Stage,
    span: Span,
    expr: &TirExpr,
    index: &TirExpr,
    value_ty: Type,
  ) -> (Port, Port) {
    let Some(index_place_trait) = self.chart.builtins.index_place else {
      let err = Port::error(
        value_ty,
        self.diags.error(Diag::MissingBuiltin { span, builtin: "IndexPlace" }),
      );
      return (err.clone(), err);
    };

    let (expr_pos, expr_neg) = self.distill_expr_place(stage, expr);
    let expr_ref = stage.ref_place(span, expr.ty, (expr_pos, expr_neg));
    let impl_type = ImplType::Trait(index_place_trait, vec![expr.ty, index.ty, value_ty]);
    let index_place_impl = self.find_impl(span, &impl_type, false);
    let assume_at_rel =
      FnRel::Item(FnId::Abstract(index_place_trait, TraitFnId(0)), vec![index_place_impl]);
    let assume_at_rel_id = self.rels.fns.push(assume_at_rel);
    let args = vec![expr_ref, self.distill_expr_value(stage, index)];
    let wire = stage.new_wire(span, value_ty);
    stage.steps.push(Step::Call(span, assume_at_rel_id, None, args, wire.neg));
    let value = stage.new_wire(span, value_ty);
    let space = stage.new_wire(span, value_ty);
    stage.steps.push(Step::Ref(wire.pos, value.neg, space.pos));
    (value.pos, space.neg)
  }

  pub(crate) fn distill_expr_poly_index(
    &mut self,
    stage: &mut Stage,
    span: Span,
    expr: &TirExpr,
    index: &TirExpr,
    value_ty: Type,
  ) -> Poly {
    Poly::Place(self.distill_expr_place_index(stage, span, expr, index, value_ty))
  }
}
