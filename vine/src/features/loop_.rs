use vine_util::parser::Parse;

use crate::{
  components::{
    distiller::{Distiller, TargetDistillation},
    lexer::Token,
    parser::Parser,
    resolver::{Resolver, TargetInfo},
  },
  structures::{
    ast::{Block, ExprKind, Label, Span, Target, Ty},
    diag::Diag,
    tir::{TargetId, TirExpr, TirExprKind},
    types::Type,
    vir::{Port, Stage},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_expr_loop(&mut self) -> Result<ExprKind, Diag> {
    self.expect(Token::Loop)?;
    let label = self.parse_label()?;
    let ty = self.parse_arrow_ty()?;
    let body = self.parse_block()?;
    Ok(ExprKind::Loop(label, ty, body))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_loop(&self, label: Label, ty: &Option<Ty>, body: &Block) -> Doc<'src> {
    Doc::concat([
      Doc("loop"),
      self.fmt_label(label),
      self.fmt_arrow_ty(ty),
      Doc(" "),
      self.fmt_block(body, true),
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_loop(
    &mut self,
    span: Span,
    label: Label,
    ty: &Option<Ty>,
    block: &Block,
  ) -> Result<TirExpr, Diag> {
    let ty = self.resolve_arrow_ty(span, ty, true);
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

impl Distiller<'_> {
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
