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
    vir::{Port, Stage, Transfer},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_expr_when(&mut self) -> Result<ExprKind, Diag> {
    self.expect(Token::When)?;
    let label = self.parse_label()?;
    let ty = self.parse_arrow_ty()?;
    self.expect(Token::OpenBrace)?;
    let mut arms = Vec::new();
    while !self.check(Token::CloseBrace) && !self.check(Token::Hole) {
      let cond = self.parse_expr()?;
      let then = self.parse_block()?;
      arms.push((cond, then));
    }
    let leg = self.eat_then(Token::Hole, Self::parse_block)?;
    self.expect(Token::CloseBrace)?;
    Ok(ExprKind::When(label, ty, arms, leg))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_when(
    &self,
    label: Label,
    ty: &Option<Ty>,
    arms: &[(Expr, Block)],
    leg: &Option<Block>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("when"),
      self.fmt_label(label),
      self.fmt_arrow_ty(ty),
      Doc(" "),
      Doc::brace_multiline(
        arms
          .iter()
          .map(|(cond, block)| {
            Doc::concat([self.fmt_expr(cond), Doc(" "), self.fmt_block(block, false)])
          })
          .chain(leg.iter().flat_map(|leg| [Doc::concat([Doc("_ "), self.fmt_block(leg, false)])])),
      ),
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_when(
    &mut self,
    span: Span,
    label: Label,
    ty: &Option<Ty>,
    arms: &[(Expr, Block)],
    leg: &Option<Block>,
  ) -> Result<TirExpr, Diag> {
    let ty = self.resolve_arrow_ty(span, ty, true);
    let target_id = self.target_id.next();
    let arms = self.bind_target(
      label.clone(),
      [Target::When],
      TargetInfo { id: target_id, break_ty: ty, continue_: true },
      |self_| {
        Vec::from_iter(arms.iter().map(|(cond, block)| {
          self_.enter_scope();
          let cond = self_.resolve_scoped_cond(cond);
          let block = self_.resolve_block_type(block, ty);
          self_.exit_scope();
          (cond, block)
        }))
      },
    );
    let leg = match leg {
      Some(leg) => Some(self.bind_target(
        label,
        [Target::When],
        TargetInfo { id: target_id, break_ty: ty, continue_: false },
        |self_| self_.resolve_block_type(leg, ty),
      )),
      None => {
        let nil = self.types.nil();
        if self.types.unify(ty, nil).is_failure() {
          self.diags.error(Diag::MissingTerminalArm { span });
        }
        None
      }
    };
    Ok(TirExpr::new(span, ty, TirExprKind::When(target_id, arms, leg)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_when(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    target_id: TargetId,
    arms: &[(TirExpr, TirExpr)],
    leg: &Option<TirExpr>,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (mut layer, mut cur_stage) = self.child_layer(stage, span);

    self.targets.get_or_extend(target_id);
    for (cond, block) in arms {
      let next_stage = self.new_unconditional_stage(&mut layer, span);
      self.targets[target_id] = Some(TargetDistillation {
        layer: layer.id,
        continue_transfer: Some(next_stage.interface),
        break_value: local,
      });
      let (mut then_stage, mut else_stage) =
        self.distill_cond(&mut layer, &mut cur_stage, span, cond);
      let result = self.distill_expr_value(&mut then_stage, block);
      then_stage.local_barrier_write_to(local, span, result);
      self.finish_stage(cur_stage);
      self.finish_stage(then_stage);
      else_stage.transfer = Some(Transfer { interface: next_stage.interface, data: None });
      self.finish_stage(else_stage);
      cur_stage = next_stage;
    }

    self.targets[target_id] =
      Some(TargetDistillation { layer: layer.id, continue_transfer: None, break_value: local });
    if let Some(leg) = leg {
      let result = self.distill_expr_value(&mut cur_stage, leg);
      cur_stage.local_barrier_write_to(local, span, result);
    }

    self.finish_stage(cur_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }
}
