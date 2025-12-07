use vine_util::parser::Parse;

use crate::{
  components::{
    distiller::Distiller,
    lexer::Token,
    matcher::Row,
    parser::{BRACE, Parser},
    resolver::Resolver,
  },
  structures::{
    ast::{Block, Expr, ExprKind, Pat, Span, Ty},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirPat},
    types::Type,
    vir::{Port, Stage},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_expr_match(&mut self) -> Result<ExprKind, Diag> {
    self.expect(Token::Match)?;
    let scrutinee = self.parse_expr()?;
    let ty = self.parse_arrow_ty()?;
    let arms = self.parse_delimited(BRACE, |self_| {
      let pat = self_.parse_pat()?;
      let value = self_.parse_block()?;
      Ok((pat, value))
    })?;
    Ok(ExprKind::Match(scrutinee, ty, arms))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_match(
    &self,
    expr: &Expr,
    ty: &Option<Ty>,
    arms: &[(Pat, Block)],
  ) -> Doc<'src> {
    Doc::concat([
      Doc("match "),
      self.fmt_expr(expr),
      self.fmt_arrow_ty(ty),
      Doc(" "),
      Doc::brace_multiline(
        arms
          .iter()
          .map(|(p, e)| Doc::concat([self.fmt_pat(p), Doc(" "), self.fmt_block(e, false)])),
      ),
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_match(
    &mut self,
    span: Span,
    scrutinee: &Expr,
    ty: &Option<Ty>,
    arms: &[(Pat, Block)],
  ) -> Result<TirExpr, Diag> {
    let scrutinee = self.resolve_expr(scrutinee);
    let ty = self.resolve_arrow_ty(span, ty, true);
    let arms = Vec::from_iter(arms.iter().map(|(pat, block)| {
      self.enter_scope();
      let pat = self.resolve_pat_type(pat, scrutinee.ty);
      let block = self.resolve_block_type(block, ty);
      self.exit_scope();
      (pat, block)
    }));
    Ok(TirExpr::new(span, ty, TirExprKind::Match(scrutinee, arms)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_match(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    value: &TirExpr,
    arms: &[(TirPat, TirExpr)],
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (mut layer, mut init_stage) = self.child_layer(stage, span);
    let value = self.distill_expr_value(&mut init_stage, value);
    let rows = arms
      .iter()
      .map(|(pat, expr)| {
        let mut stage = self.new_unconditional_stage(&mut layer, span);
        let result = self.distill_expr_value(&mut stage, expr);
        stage.local_barrier_write_to(local, result);
        let interface = stage.interface;
        self.finish_stage(stage);
        Row::new(Some(pat), interface)
      })
      .collect();
    self.distill_pattern_match(span, &mut layer, &mut init_stage, value, rows);
    self.finish_stage(init_stage);
    self.finish_layer(layer);
    stage.local_read_barrier(local, span, ty)
  }
}
