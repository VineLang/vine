use vine_util::parser::Parse;

use crate::{
  components::{
    distiller::{Distiller, TargetDistillation},
    lexer::Token,
    parser::Parser,
    resolver::{Resolver, TargetInfo},
  },
  structures::{
    ast::{Block, Expr, ExprKind, Label, Span, Target, Ty},
    diag::Diag,
    tir::{TargetId, TirExpr, TirExprKind},
    types::Type,
    vir::{Port, PortKind, Stage, Step, Transfer},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_expr_while(&mut self) -> Result<ExprKind, Diag> {
    self.expect(Token::While)?;
    let label = self.parse_label()?;
    let cond = self.parse_expr()?;
    let ty = self.parse_arrow_ty()?;
    let body = self.parse_block()?;
    let else_ = self.eat_then(Token::Else, Self::parse_block)?;
    Ok(ExprKind::While(label, cond, ty, body, else_))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_while(
    &self,
    label: Label,
    cond: &Expr,
    ty: &Option<Ty>,
    body: &Block,
    else_: &Option<Block>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("while"),
      self.fmt_label(label),
      self.fmt_arrow_ty(ty),
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

impl Resolver<'_> {
  pub(crate) fn resolve_expr_while(
    &mut self,
    span: Span,
    label: Label,
    cond: &Expr,
    ty: &Option<Ty>,
    block: &Block,
    else_: &Option<Block>,
  ) -> Result<TirExpr, Diag> {
    let ty = self.resolve_arrow_ty(span, ty, true);
    let target_id = self.target_id.next();
    let (cond, block, else_) = self.bind_target(
      label,
      [Target::AnyLoop, Target::While],
      TargetInfo { id: target_id, break_ty: ty, continue_: true },
      |self_| {
        self_.enter_scope();
        let cond = self_.resolve_scoped_cond(cond);
        let block = self_.resolve_block_nil(block);
        self_.exit_scope();
        let else_ = else_.as_ref().map(|b| self_.resolve_block_type(b, ty));
        let nil = self_.types.nil();
        if else_.is_none() && self_.types.unify(ty, nil).is_failure() {
          self_.diags.error(Diag::MissingElse { span });
        }
        (cond, block, else_)
      },
    );
    Ok(TirExpr::new(span, ty, TirExprKind::While(target_id, cond, block, else_)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_while(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    label: TargetId,
    cond: &TirExpr,
    block: &TirExpr,
    else_: &Option<TirExpr>,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    stage.local_barrier(local);
    let (mut layer, mut cond_stage) = self.child_layer(stage, span);

    *self.targets.get_or_extend(label) = Some(TargetDistillation {
      layer: layer.id,
      continue_transfer: Some(cond_stage.interface),
      break_value: local,
    });

    let (mut then_stage, mut else_stage) =
      self.distill_cond(&mut layer, &mut cond_stage, span, cond);

    let result = self.distill_expr_value(&mut then_stage, block);
    then_stage.steps.push(Step::Link(result, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    then_stage.transfer = Some(Transfer::unconditional(cond_stage.interface));

    if let Some(else_) = else_ {
      let result = self.distill_expr_value(&mut else_stage, else_);
      else_stage.local_barrier_write_to(local, result);
    }

    self.finish_stage(cond_stage);
    self.finish_stage(then_stage);
    self.finish_stage(else_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }
}
