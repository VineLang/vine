use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::{Distiller, Label},
    lexer::Token,
    parser::VineParser,
    resolver::Resolver,
  },
  structures::{
    ast::{Block, ExprKind, Ident, Span},
    diag::Diag,
    tir::{LabelId, TirExpr, TirExprKind},
    types::Type,
    vir::{Port, PortKind, Stage, Step, Transfer},
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
  pub(crate) fn fmt_expr_loop(
    &self,
    label: Option<Ident<'core>>,
    body: &Block<'core>,
  ) -> Doc<'src> {
    Doc::concat([Doc("loop"), self.fmt_label(label), Doc(" "), self.fmt_block(body, true)])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_loop(
    &mut self,
    span: Span,
    label: Option<Ident<'core>>,
    block: &Block<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let result = self.types.new_var(span);
    let (label, block) = self.bind_label(label, true, result, |self_| self_.resolve_block(block));
    Ok(TirExpr::new(span, result, TirExprKind::Loop(label, block)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_loop(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    label: LabelId,
    block: &TirExpr,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (layer, mut body_stage) = self.child_layer(stage, span);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(body_stage.interface),
      break_value: Some(local),
    });

    let result = self.distill_expr_value(&mut body_stage, block);
    body_stage.steps.push(Step::Link(result, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    body_stage.transfer = Some(Transfer::unconditional(body_stage.interface));

    self.finish_stage(body_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }
}
