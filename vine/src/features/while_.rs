use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::{Distiller, Label},
    lexer::Token,
    parser::VineParser,
    resolver::Resolver,
  },
  structures::{
    ast::{Block, Expr, ExprKind, Ident, Span},
    diag::Diag,
    tir::{LabelId, TirExpr, TirExprKind},
    vir::{Port, PortKind, Stage, Step, Transfer},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_expr_while(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    self.expect(Token::While)?;
    let label = self.parse_label()?;
    let cond = self.parse_expr()?;
    let body = self.parse_block()?;
    Ok(ExprKind::While(label, cond, body))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_while(
    &self,
    label: Option<Ident<'core>>,
    cond: &Expr<'core>,
    body: &Block<'core>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("while"),
      self.fmt_label(label),
      Doc(" "),
      self.fmt_expr(cond),
      Doc(" "),
      self.fmt_block(body, true),
    ])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_while(
    &mut self,
    span: Span,
    label: Option<Ident<'core>>,
    cond: &Expr<'core>,
    block: &Block<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let nil = self.types.nil();
    let (label, (cond, block)) = self.bind_label(label, true, nil, |self_| {
      self_.enter_scope();
      let cond = self_.resolve_scoped_cond(cond);
      let block = self_.resolve_block(block);
      self_.exit_scope();
      (cond, block)
    });
    Ok(TirExpr::new(span, nil, TirExprKind::While(label, cond, block)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_while(
    &mut self,
    stage: &mut Stage,
    span: Span,
    label: LabelId,
    cond: &TirExpr,
    block: &TirExpr,
  ) {
    let (mut layer, mut cond_stage) = self.child_layer(stage, span);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(cond_stage.interface),
      break_value: None,
    });

    let (mut then_stage, else_stage) = self.distill_cond(&mut layer, &mut cond_stage, span, cond);

    let result = self.distill_expr_value(&mut then_stage, block);
    then_stage.steps.push(Step::Link(result, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    then_stage.transfer = Some(Transfer::unconditional(cond_stage.interface));

    self.finish_stage(cond_stage);
    self.finish_stage(then_stage);
    self.finish_stage(else_stage);
    self.finish_layer(layer);
  }
}
