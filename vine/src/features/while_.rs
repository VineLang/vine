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
    types::Type,
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
    let else_ = self.eat(Token::Else)?.then(|| self.parse_block()).transpose()?;
    Ok(ExprKind::While(label, cond, body, else_))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_while(
    &self,
    label: Option<Ident<'core>>,
    cond: &Expr<'core>,
    body: &Block<'core>,
    else_: &Option<Block<'core>>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("while"),
      self.fmt_label(label),
      Doc(" "),
      self.fmt_expr(cond),
      Doc(" "),
      self.fmt_block(body, true),
      match else_ {
        Some(else_) => Doc::concat([Doc(" else "), self.fmt_block(else_, true)]),
        None => Doc(""),
      },
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
    else_: &Option<Block<'core>>,
  ) -> Result<TirExpr, Diag<'core>> {
    let result = if else_.is_some() { self.types.new_var(span) } else { self.types.nil() };
    let (label, (cond, block, else_)) = self.bind_label(label, true, result, |self_| {
      self_.enter_scope();
      let cond = self_.resolve_scoped_cond(cond);
      let block = self_.resolve_block_nil(block);
      self_.exit_scope();
      let else_ = else_.as_ref().map(|b| self_.resolve_block_type(b, result));
      (cond, block, else_)
    });
    Ok(TirExpr::new(span, result, TirExprKind::While(label, cond, block, else_)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_while(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    label: LabelId,
    cond: &TirExpr,
    block: &TirExpr,
    else_: &Option<TirExpr>,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    stage.local_barrier(local);
    let (mut layer, mut cond_stage) = self.child_layer(stage, span);

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(cond_stage.interface),
      break_value: Some(local),
    });

    let (mut then_stage, mut else_stage) =
      self.distill_cond(&mut layer, &mut cond_stage, span, cond);

    let result = self.distill_expr_value(&mut then_stage, block);
    then_stage.steps.push(Step::Link(result, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    then_stage.transfer = Some(Transfer::unconditional(cond_stage.interface));

    if let Some(else_) = else_ {
      let result = self.distill_expr_value(&mut else_stage, else_);
      else_stage.local_barrier_write_to(local, result);
    } else {
      else_stage.local_barrier_write_to(local, Port { ty: self.types.nil(), kind: PortKind::Nil });
    }

    self.finish_stage(cond_stage);
    self.finish_stage(then_stage);
    self.finish_stage(else_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }
}
