use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::{Distiller, TargetDistillation},
    lexer::Token,
    parser::VineParser,
    resolver::{Resolver, TargetInfo},
  },
  structures::{
    ast::{Block, ExprKind, Label, Span, Target, Ty},
    diag::Diag,
    tir::{TargetId, TirExpr, TirExprKind},
    vir::{Port, Stage},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_expr_do(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    self.expect(Token::Do)?;
    let label = self.parse_label()?;
    let ty = self.parse_arrow_ty()?;
    let block = self.parse_block()?;
    Ok(ExprKind::Do(label, ty, block))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_do(
    &self,
    label: Label<'core>,
    ty: &Option<Ty<'core>>,
    body: &Block<'core>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("do"),
      self.fmt_label(label),
      self.fmt_arrow_ty(ty),
      Doc(" "),
      self.fmt_block(body, false),
    ])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_do(
    &mut self,
    span: Span,
    label: Label<'core>,
    ty: &Option<Ty<'core>>,
    block: &Block<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let ty = self.resolve_arrow_ty(span, ty, true);
    let target_id = self.target_id.next();
    let block = self.bind_target(
      label,
      [Target::Do],
      TargetInfo { id: target_id, break_ty: ty, continue_: false },
      |self_| self_.resolve_block_type(block, ty),
    );
    Ok(TirExpr::new(span, ty, TirExprKind::Do(target_id, block)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_do(
    &mut self,
    stage: &mut Stage,
    span: Span,
    label: TargetId,
    block: &TirExpr,
  ) -> Port {
    let local = self.new_local(stage, span, block.ty);
    let (layer, mut body_stage) = self.child_layer(stage, span);

    *self.targets.get_or_extend(label) =
      Some(TargetDistillation { layer: layer.id, continue_transfer: None, break_value: local });

    let result = self.distill_expr_value(&mut body_stage, block);
    body_stage.local_barrier_write_to(local, result);

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, block.ty)
  }
}
