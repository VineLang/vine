use crate::{
  components::{distiller::Distiller, matcher::Row, resolver::Resolver},
  structures::{
    ast::{Expr, Span},
    chart::VariantId,
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirLocal, TirPat, TirPatKind},
    types::{Type, TypeKind},
    vir::{Port, Stage, Step},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_try(&self, expr: &Expr) -> Doc<'src> {
    Doc::concat([self.fmt_expr(expr), Doc(".try")])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_try(&mut self, span: Span, result: &Expr) -> Result<TirExpr, Diag> {
    let Some(result_id) = self.chart.builtins.result else {
      Err(Diag::MissingBuiltin { span, builtin: "Result" })?
    };
    let ok = self.types.new_var(span);
    let err = self.types.new_var(span);
    let result_ty = self.types.new(TypeKind::Enum(result_id, vec![ok, err]));
    let result = self.resolve_expr(result);
    if let Some(err) = self.expect_type(result.span, result.ty, result_ty) {
      Err(err)?
    }
    let return_ok = self.types.new_var(span);
    let return_result = self.types.new(TypeKind::Enum(result_id, vec![return_ok, err]));
    if let Some(return_ty) = &self.return_ty {
      if self.types.unify(*return_ty, return_result).is_failure() {
        self.diags.error(Diag::TryBadReturnType {
          span,
          tried: self.types.show(self.chart, result_ty),
          ret: self.types.show(self.chart, *return_ty),
        });
      }
    } else {
      Err(Diag::NoReturn { span })?
    }
    Ok(TirExpr::new(span, ok, TirExprKind::Try(ok, err, result)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_try(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    ok_ty: Type,
    err_ty: Type,
    result: &TirExpr,
  ) -> Port {
    let result_id = self.chart.builtins.result.unwrap();
    let ok_variant = VariantId(0);
    let err_variant = VariantId(1);

    let (mut layer, mut init_stage) = self.child_layer(stage, span);
    let result = self.distill_expr_value(&mut init_stage, result);
    let mut ok_stage = self.new_unconditional_stage(&mut layer, span);
    let mut err_stage = self.new_unconditional_stage(&mut layer, span);
    let ok_local = self.locals.push(TirLocal { span, ty: ok_ty });
    let err_local = self.locals.push(TirLocal { span, ty: err_ty });
    let pat = |variant_id, content_ty, local| TirPat {
      span,
      ty,
      kind: Box::new(TirPatKind::Enum(
        result_id,
        variant_id,
        TirPat { span, ty: content_ty, kind: Box::new(TirPatKind::Local(local)) },
      )),
    };

    self.distill_pattern_match(
      span,
      &mut layer,
      &mut init_stage,
      result,
      vec![
        Row::new(Some(&pat(ok_variant, ok_ty, ok_local)), ok_stage.interface),
        Row::new(Some(&pat(err_variant, err_ty, err_local)), err_stage.interface),
      ],
    );
    self.finish_stage(init_stage);

    let out_local = self.new_local(stage, span, ty);
    let value = ok_stage.local_read_barrier(ok_local, span, ok_ty);
    ok_stage.local_barrier_write_to(out_local, value);
    self.finish_stage(ok_stage);

    let ret = self.returns.last().unwrap();
    let err = err_stage.local_read_barrier(err_local, span, err_ty);
    let result = err_stage.new_wire(span, ret.ty);
    err_stage.steps.push(Step::Enum(result_id, err_variant, result.neg, err));
    err_stage.local_barrier_write_to(ret.local, result.pos);
    err_stage.steps.push(Step::Diverge(ret.layer, None));
    self.finish_stage(err_stage);

    self.finish_layer(layer);
    stage.local_read_barrier(out_local, span, ty)
  }
}
