use std::mem::take;

use vine_util::parser::Parse;

use crate::{
  components::{
    distiller::{Distiller, Return},
    lexer::Token,
    parser::Parser,
    resolver::Resolver,
  },
  structures::{
    ast::{Block, ExprKind, Flex, Pat, Span, Ty},
    content::{Content, Keyword, Space},
    diag::Diag,
    signatures::FnSig,
    tir::{Local, TirClosure, TirExpr, TirExprKind, TirLocal},
    types::TypeKind,
    vir::Stage,
  },
  tools::fmt::Formatter,
};

impl Parser<'_> {
  pub(crate) fn parse_expr_bench(&mut self) -> Result<ExprKind, Diag> {
    self.expect(Token::Fn)?;
    let flex = self.parse_flex()?;
    let params = self.parse_pats()?;
    let ty = self.parse_arrow_ty()?;
    let body = self.parse_block()?;
    Ok(ExprKind::Bench(flex, params, ty, body))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_bench(
    &self,
    flex: &Flex,
    params: &[Pat],
    ty: &Option<Ty>,
    body: &Block,
  ) -> Content {
    Content::even((Keyword("$bench"), Space, self.fmt_expr_fn(flex, params, ty, body)))
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_bench(
    &mut self,
    span: Span,
    flex: Flex,
    params: &[Pat],
    ret: &Option<Ty>,
    body: &Block,
  ) -> Result<TirExpr, Diag> {
    let old_targets = take(&mut self.targets);
    self.enter_scope();
    let names = params.iter().map(|x| self.param_name(x)).collect();
    let params = params.iter().map(|p| self.resolve_pat(p)).collect::<Vec<_>>();
    let param_tys = params.iter().map(|x| x.ty).collect();
    let ret_ty = ret
      .as_ref()
      .map(|t| self.resolve_ty(t, true))
      .unwrap_or_else(|| if true { self.types.new_var(body.span) } else { self.types.nil() });
    let old_return_ty = self.return_ty.replace(ret_ty);
    let body = self.resolve_block_type(body, ret_ty);
    self.exit_scope();
    self.targets = old_targets;
    self.return_ty = old_return_ty;

    let local = self.locals.push(TirLocal { span, ty: ret_ty });
    let nil = self.types.nil();

    let inner_closure = self.closures.push(None);
    let inner_ty = self.types.new(TypeKind::Closure(
      inner_closure,
      Flex::None,
      FnSig { names: vec![], param_tys: vec![], ret_ty },
    ));
    let inner_body = TirExpr::new(span, ret_ty, TirExprKind::Local(local));
    self.closures[inner_closure] =
      Some(TirClosure { span, ty: inner_ty, flex: Flex::None, params: vec![], body: inner_body });

    let outer_closure = self.closures.push(None);
    let outer_ty = self.types.new(TypeKind::Closure(
      outer_closure,
      flex,
      FnSig { names, param_tys, ret_ty: inner_ty },
    ));
    let outer_body = TirExpr::new(
      span,
      inner_ty,
      TirExprKind::Seq(
        TirExpr::new(span, nil, TirExprKind::CaptureReturn(local, body)),
        TirExpr::new(span, inner_ty, TirExprKind::Closure(inner_closure)),
      ),
    );
    self.closures[outer_closure] =
      Some(TirClosure { span, ty: outer_ty, flex, params, body: outer_body });

    self.benches.push((inner_closure, outer_closure));

    Ok(TirExpr::new(span, outer_ty, TirExprKind::Closure(outer_closure)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_nil_capture_return(
    &mut self,
    stage: &mut Stage,
    span: Span,
    local: Local,
    body: &TirExpr,
  ) {
    stage.declarations.push(local);
    let (layer, mut body_stage) = self.child_layer(stage, span);
    self.returns.push(Return { ty: self.locals[local].ty, layer: layer.id, local });
    let result = self.distill_expr_value(&mut body_stage, body);
    body_stage.local_barrier_write_to(local, span, result);
    self.finish_stage(body_stage);
    self.finish_layer(layer);
  }
}
