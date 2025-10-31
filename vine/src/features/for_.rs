use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::{Distiller, TargetDistillation},
    lexer::Token,
    matcher::Row,
    parser::VineParser,
    resolver::{Resolver, TargetInfo},
  },
  structures::{
    ast::{Block, Expr, ExprKind, Label, Pat, Span, Target, Ty},
    chart::VariantId,
    diag::Diag,
    resolutions::FnRelId,
    tir::{TargetId, TirExpr, TirExprKind, TirLocal, TirPat, TirPatKind},
    types::{Type, TypeKind},
    vir::{Port, PortKind, Stage, Step, Transfer},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl VineParser<'_> {
  pub(crate) fn parse_expr_for(&mut self) -> Result<ExprKind, Diag> {
    self.expect(Token::For)?;
    let label = self.parse_label()?;
    let pat = self.parse_pat()?;
    self.expect(Token::In)?;
    let expr = self.parse_expr()?;
    let ty = self.parse_arrow_ty()?;
    let body = self.parse_block()?;
    let else_ = self.eat_then(Token::Else, Self::parse_block)?;
    Ok(ExprKind::For(label, pat, expr, ty, body, else_))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_for(
    &self,
    label: Label,
    pat: &Pat,
    expr: &Expr,
    ty: &Option<Ty>,
    block: &Block,
    else_: &Option<Block>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("for"),
      self.fmt_label(label),
      Doc(" "),
      self.fmt_pat(pat),
      Doc(" in "),
      self.fmt_expr(expr),
      self.fmt_arrow_ty(ty),
      Doc(" "),
      self.fmt_block(block, true),
      match else_ {
        Some(else_) => Doc::concat([Doc(" else "), self.fmt_block(else_, true)]),
        None => Doc(""),
      },
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_for(
    &mut self,
    span: Span,
    label: Label,
    pat: &Pat,
    iter: &Expr,
    ty: &Option<Ty>,
    block: &Block,
    else_: &Option<Block>,
  ) -> Result<TirExpr, Diag> {
    let ty = self.resolve_arrow_ty(span, ty, true);
    let target_id = self.target_id.next();
    let result = self.bind_target(
      label,
      [Target::AnyLoop, Target::For],
      TargetInfo { id: target_id, break_ty: ty, continue_: true },
      |self_| {
        self_.enter_scope();
        let iter = self_.resolve_expr(iter);
        let pat = self_.resolve_pat(pat);
        let rel =
          self_.builtin_fn(span, self_.chart.builtins.advance, "advance", [iter.ty, pat.ty])?;
        let block = self_.resolve_block_nil(block);
        self_.exit_scope();
        let else_ = else_.as_ref().map(|b| self_.resolve_block_type(b, ty));
        let nil = self_.types.nil();
        if else_.is_none() && self_.types.unify(ty, nil).is_failure() {
          self_.core.report(Diag::MissingElse { span });
        }
        Result::<_, Diag>::Ok((rel, pat, iter, block, else_))
      },
    );
    let (rel, pat, iter, block, else_) = result?;
    Ok(TirExpr::new(span, ty, TirExprKind::For(target_id, rel, pat, iter, block, else_)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_for(
    &mut self,
    stage: &mut Stage,
    span: Span,
    result_ty: Type,
    target_id: TargetId,
    rel: FnRelId,
    pat: &TirPat,
    iter: &TirExpr,
    block: &TirExpr,
    else_: &Option<TirExpr>,
  ) -> Port {
    let Some(option_enum) = self.chart.builtins.option else {
      let err = self.core.report(Diag::MissingBuiltin { span, builtin: "Option" });
      return Port::error(result_ty, err);
    };

    let result_local = self.new_local(stage, span, result_ty);
    stage.local_barrier(result_local);

    let value_ty = pat.ty;
    let iter_ty = iter.ty;
    let tuple_ty = self.types.new(TypeKind::Tuple(vec![value_ty, iter_ty]));
    let option_ty = self.types.new(TypeKind::Enum(option_enum, vec![tuple_ty]));
    let some_variant = VariantId(0);
    let none_variant = VariantId(1);
    let iter_local = self.new_local(stage, span, iter_ty);
    let value_local = self.locals.push(TirLocal { span, ty: value_ty });
    let inner_iter_local = self.locals.push(TirLocal { span, ty: iter_ty });

    let iter = self.distill_expr_value(stage, iter);
    stage.local_write_to(iter_local, iter);

    let (mut layer, mut init_stage) = self.child_layer(stage, span);
    let mut some_stage = self.new_unconditional_stage(&mut layer, span);
    let mut none_stage = self.new_unconditional_stage(&mut layer, span);

    let iter = init_stage.local_read_barrier(iter_local, span, iter_ty);
    let option = init_stage.new_wire(span, option_ty);
    init_stage.steps.push(Step::Call(span, rel, None, vec![iter], option.neg));
    self.distill_pattern_match(
      span,
      &mut layer,
      &mut init_stage,
      option.pos,
      vec![
        Row::new(
          Some(&TirPat {
            span,
            ty: option_ty,
            kind: Box::new(TirPatKind::Enum(
              option_enum,
              some_variant,
              Some(TirPat {
                span,
                ty: tuple_ty,
                kind: Box::new(TirPatKind::Composite(vec![
                  TirPat { span, ty: value_ty, kind: Box::new(TirPatKind::Local(value_local)) },
                  TirPat { span, ty: iter_ty, kind: Box::new(TirPatKind::Local(inner_iter_local)) },
                ])),
              }),
            )),
          }),
          some_stage.interface,
        ),
        Row::new(
          Some(&TirPat {
            span,
            ty: option_ty,
            kind: Box::new(TirPatKind::Enum(option_enum, none_variant, None)),
          }),
          none_stage.interface,
        ),
      ],
    );

    *self.targets.get_or_extend(target_id) = Some(TargetDistillation {
      layer: layer.id,
      continue_transfer: Some(init_stage.interface),
      break_value: result_local,
    });

    let iter = some_stage.local_read_barrier(inner_iter_local, span, iter_ty);
    some_stage.local_barrier_write_to(iter_local, iter);
    let pat = self.distill_pat_value(&mut some_stage, pat);
    some_stage.local_read_barrier_to(value_local, pat);
    let result = self.distill_expr_value(&mut some_stage, block);
    some_stage.steps.push(Step::Link(result, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    some_stage.transfer = Some(Transfer::unconditional(init_stage.interface));

    if let Some(else_) = else_ {
      let result = self.distill_expr_value(&mut none_stage, else_);
      none_stage.local_barrier_write_to(result_local, result);
    }

    self.finish_stage(init_stage);
    self.finish_stage(some_stage);
    self.finish_stage(none_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(result_local, span, result_ty)
  }
}
