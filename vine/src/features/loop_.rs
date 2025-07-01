use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::{Distiller, TargetDistillation},
    lexer::Token,
    parser::VineParser,
    resolver::{Resolver, TargetInfo},
  },
  structures::{
    ast::{Block, ExprKind, Label, Span, Target},
    diag::Diag,
    tir::{TargetId, TirExpr, TirExprKind},
    types::Type,
    vir::{Port, Stage},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_expr_loop(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    self.expect(Token::Loop)?;
    let label = self.parse_label()?;
    let body = self.parse_block()?;
    Ok(ExprKind::Loop(label, body))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_loop(&self, label: Label<'core>, body: &Block<'core>) -> Doc<'src> {
    Doc::concat([Doc("loop"), self.fmt_label(label), Doc(" "), self.fmt_block(body, true)])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_loop(
    &mut self,
    span: Span,
    label: Label<'core>,
    block: &Block<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let ty = self.types.new_var(span);
    let target_id = self.target_id.next();
    let block = self.bind_target(
      label,
      [Target::AnyLoop, Target::Loop],
      TargetInfo { id: target_id, break_ty: ty, continue_: true },
      |self_| self_.resolve_block_type(block, ty),
    );
    Ok(TirExpr::new(span, ty, TirExprKind::Loop(target_id, block)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_loop(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    label: TargetId,
    block: &TirExpr,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (layer, mut body_stage) = self.child_layer(stage, span);

    *self.targets.get_or_extend(label) = Some(TargetDistillation {
      layer: layer.id,
      continue_transfer: Some(body_stage.interface),
      break_value: local,
    });

    let result = self.distill_expr_value(&mut body_stage, block);
    body_stage.local_barrier_write_to(local, result);

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }
}
