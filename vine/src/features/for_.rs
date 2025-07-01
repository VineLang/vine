use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::{Distiller, Label},
    lexer::Token,
    matcher::Row,
    parser::VineParser,
    resolver::Resolver,
  },
  structures::{
    ast::{Block, Expr, ExprKind, Ident, Pat, Span},
    chart::VariantId,
    diag::Diag,
    resolutions::FnRelId,
    tir::{LabelId, TirExpr, TirExprKind, TirLocal, TirPat, TirPatKind},
    types::TypeKind,
    vir::{Port, PortKind, Stage, Step, Transfer},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_expr_for(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    self.expect(Token::For)?;
    let label = self.parse_label()?;
    let pat = self.parse_pat()?;
    self.expect(Token::In)?;
    let expr = self.parse_expr()?;
    let body = self.parse_block()?;
    Ok(ExprKind::For(label, pat, expr, body))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_for(
    &self,
    label: Option<Ident<'core>>,
    pat: &Pat<'core>,
    expr: &Expr<'core>,
    block: &Block<'core>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("for"),
      self.fmt_label(label),
      Doc(" "),
      self.fmt_pat(pat),
      Doc(" in "),
      self.fmt_expr(expr),
      Doc(" "),
      self.fmt_block(block, true),
    ])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_for(
    &mut self,
    span: Span,
    label: Option<Ident<'core>>,
    pat: &Pat<'core>,
    iter: &Expr<'core>,
    block: &Block<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let nil = self.types.nil();
    let (label, result) = self.bind_label(label, true, nil, |self_| {
      self_.enter_scope();
      let iter = self_.resolve_expr(iter);
      let pat = self_.resolve_pat(pat);
      let rel =
        self_.builtin_fn(span, self_.chart.builtins.advance, "advance", [iter.ty, pat.ty])?;
      let block = self_.resolve_block(block);
      self_.exit_scope();
      Result::<_, Diag<'core>>::Ok((rel, pat, iter, block))
    });
    let (rel, pat, iter, block) = result?;
    Ok(TirExpr::new(span, nil, TirExprKind::For(label, rel, pat, iter, block)))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_for(
    &mut self,
    stage: &mut Stage,
    span: Span,
    label: LabelId,
    rel: FnRelId,
    pat: &TirPat,
    iter: &TirExpr,
    block: &TirExpr,
  ) {
    let Some(option_enum) = self.chart.builtins.option else {
      self.core.report(Diag::MissingBuiltin { span, builtin: "Option" });
      return;
    };

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
    let none_stage = self.new_unconditional_stage(&mut layer, span);

    let iter = init_stage.local_read_barrier(iter_local, span, iter_ty);
    let option = init_stage.new_wire(span, option_ty);
    init_stage.steps.push(Step::Call(rel, None, vec![iter], option.neg));
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

    *self.labels.get_or_extend(label) = Some(Label {
      layer: layer.id,
      continue_transfer: Some(init_stage.interface),
      break_value: None,
    });

    let iter = some_stage.local_read_barrier(inner_iter_local, span, iter_ty);
    some_stage.local_barrier_write_to(iter_local, iter);
    let pat = self.distill_pat_value(&mut some_stage, pat);
    some_stage.local_read_barrier_to(value_local, pat);
    let result = self.distill_expr_value(&mut some_stage, block);
    some_stage.steps.push(Step::Link(result, Port { ty: self.types.nil(), kind: PortKind::Nil }));
    some_stage.transfer = Some(Transfer::unconditional(init_stage.interface));

    self.finish_stage(init_stage);
    self.finish_stage(some_stage);
    self.finish_stage(none_stage);
    self.finish_layer(layer);
  }
}
