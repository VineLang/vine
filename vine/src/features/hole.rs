use crate::{
  components::{distiller::Distiller, resolver::Resolver},
  structures::{
    ast::Span,
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::Type,
    vir::{Port, Stage},
  },
};

impl Resolver<'_> {
  pub(crate) fn resolve_expr_hole(&mut self, span: Span) -> Result<TirExpr, Diag> {
    Ok(TirExpr::new(span, self.types.new_var(span), TirExprKind::Hole))
  }

  pub(crate) fn resolve_pat_hole(&mut self, span: Span) -> Result<TirPat, Diag> {
    Ok(TirPat::new(span, self.types.new_var(span), TirPatKind::Hole))
  }

  pub(crate) fn resolve_ty_hole(&mut self, span: Span, inference: bool) -> Type {
    if inference {
      self.types.new_var(span)
    } else {
      self.types.error(self.core.report(Diag::ItemTypeHole { span }))
    }
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_space_hole(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
  ) -> Port {
    self.drop_space(span, stage, ty)
  }

  pub(crate) fn distill_pat_value_hole(&mut self, stage: &mut Stage, span: Span, ty: Type) -> Port {
    self.drop_space(span, stage, ty)
  }

  pub(crate) fn distill_pat_space_hole(&mut self, stage: &mut Stage, span: Span, ty: Type) -> Port {
    self.drop_space(span, stage, ty.inverse())
  }

  pub(crate) fn distill_pat_place_hole(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
  ) -> (Port, Port) {
    let wire = stage.new_wire(span, ty);
    (wire.neg, wire.pos)
  }
}
