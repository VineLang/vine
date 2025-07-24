use ivy::ast::Tree;
use vine_util::{idx::IdxVec, parser::Parser};

use crate::{
  components::{
    distiller::Distiller,
    emitter::Emitter,
    lexer::Token,
    matcher::{MatchVar, MatchVarForm, MatchVarKind, Matcher, Row, VarId},
    parser::{VineParser, BP},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Pat, PatKind, Span, Ty, TyKind},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::{Type, TypeKind},
    vir::{Layer, Port, Stage, Step},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_expr_ref(&mut self, span: usize) -> Result<ExprKind<'core>, Diag<'core>> {
    if self.eat(Token::And)? {
      Ok(ExprKind::Ref(self.parse_expr_bp(BP::Prefix)?, false))
    } else {
      self.expect(Token::AndAnd)?;
      let inner = self.parse_expr_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      Ok(ExprKind::Ref(Expr { span, kind: Box::new(ExprKind::Ref(inner, false)) }, false))
    }
  }

  pub(crate) fn parse_expr_deref(&mut self, span: usize) -> Result<ExprKind<'core>, Diag<'core>> {
    if self.eat(Token::Star)? {
      Ok(ExprKind::Deref(self.parse_expr_bp(BP::Prefix)?, false))
    } else {
      self.expect(Token::StarStar)?;
      let inner = self.parse_expr_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      Ok(ExprKind::Deref(Expr { span, kind: Box::new(ExprKind::Deref(inner, false)) }, false))
    }
  }

  pub(crate) fn parse_pat_ref(&mut self, span: usize) -> Result<PatKind<'core>, Diag<'core>> {
    if self.eat(Token::And)? {
      Ok(PatKind::Ref(self.parse_pat_bp(BP::Prefix)?))
    } else {
      self.expect(Token::AndAnd)?;
      let inner = self.parse_pat_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      Ok(PatKind::Ref(Pat { span, kind: Box::new(PatKind::Ref(inner)) }))
    }
  }

  pub(crate) fn parse_pat_deref(&mut self, span: usize) -> Result<PatKind<'core>, Diag<'core>> {
    if self.eat(Token::Star)? {
      Ok(PatKind::Deref(self.parse_pat_bp(BP::Prefix)?))
    } else {
      self.expect(Token::StarStar)?;
      let inner = self.parse_pat_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      Ok(PatKind::Deref(Pat { span, kind: Box::new(PatKind::Deref(inner)) }))
    }
  }

  pub(crate) fn parse_ty_ref(&mut self, span: usize) -> Result<TyKind<'core>, Diag<'core>> {
    if self.eat(Token::And)? {
      Ok(TyKind::Ref(self.parse_ty_bp(BP::Prefix)?))
    } else {
      self.expect(Token::AndAnd)?;
      let inner = self.parse_ty_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      Ok(TyKind::Ref(Ty { span, kind: Box::new(TyKind::Ref(inner)) }))
    }
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_ref(&self, expr: &Expr<'core>, postfix: bool) -> Doc<'src> {
    if postfix {
      Doc::concat([self.fmt_expr(expr), Doc(".&")])
    } else {
      Doc::concat([Doc("&"), self.fmt_expr(expr)])
    }
  }

  pub(crate) fn fmt_expr_deref(&self, expr: &Expr<'core>, postfix: bool) -> Doc<'src> {
    if postfix {
      Doc::concat([self.fmt_expr(expr), Doc(".*")])
    } else {
      Doc::concat([Doc("*"), self.fmt_expr(expr)])
    }
  }

  pub(crate) fn fmt_pat_ref(&self, pat: &Pat<'core>) -> Doc<'src> {
    Doc::concat([Doc("&"), self.fmt_pat(pat)])
  }

  pub(crate) fn fmt_pat_deref(&self, pat: &Pat<'core>) -> Doc<'src> {
    Doc::concat([Doc("*"), self.fmt_pat(pat)])
  }

  pub(crate) fn fmt_ty_ref(&self, ty: &Ty<'core>) -> Doc<'src> {
    Doc::concat([Doc("&"), self.fmt_ty(ty)])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_ref(
    &mut self,
    span: Span,
    inner: &Expr<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let inner = self.resolve_expr(inner);
    Ok(TirExpr::new(span, self.types.new(TypeKind::Ref(inner.ty)), TirExprKind::Ref(inner)))
  }

  pub(crate) fn resolve_expr_deref(
    &mut self,
    span: Span,
    inner: &Expr<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let ty = self.types.new_var(span);
    let ref_ty = self.types.new(TypeKind::Ref(ty));
    let inner = self.resolve_expr_type(inner, ref_ty);
    Ok(TirExpr::new(span, ty, TirExprKind::Deref(inner)))
  }

  pub(crate) fn resolve_pat_ref(
    &mut self,
    span: Span,
    inner: &Pat<'core>,
  ) -> Result<TirPat, Diag<'core>> {
    let inner = self.resolve_pat(inner);
    Ok(TirPat::new(span, self.types.new(TypeKind::Ref(inner.ty)), TirPatKind::Ref(inner)))
  }

  pub(crate) fn resolve_pat_sig_ref(&mut self, inner: &Pat<'core>, inference: bool) -> Type {
    let inner = self.resolve_pat_sig(inner, inference);
    self.types.new(TypeKind::Ref(inner))
  }

  pub(crate) fn resolve_pat_deref(
    &mut self,
    span: Span,
    inner: &Pat<'core>,
  ) -> Result<TirPat, Diag<'core>> {
    let ty = self.types.new_var(span);
    let ref_ty = self.types.new(TypeKind::Ref(ty));
    let inner = self.resolve_pat_type(inner, ref_ty);
    Ok(TirPat::new(span, ty, TirPatKind::Deref(inner)))
  }

  pub(crate) fn resolve_ty_ref(&mut self, inner: &Ty<'core>, inference: bool) -> Type {
    let inner = self.resolve_ty(inner, inference);
    self.types.new(TypeKind::Ref(inner))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_expr_value_ref(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    place: &TirExpr,
  ) -> Port {
    let place = self.distill_expr_place(stage, place);
    stage.ref_place(span, ty, place)
  }

  pub(crate) fn distill_expr_place_deref(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    reference: &TirExpr,
  ) -> (Port, Port) {
    let reference = self.distill_expr_value(stage, reference);
    let value = stage.new_wire(span, ty);
    let space = stage.new_wire(span, ty);
    stage.steps.push(Step::Ref(reference, value.neg, space.pos));
    (value.pos, space.neg)
  }

  pub(crate) fn distill_pat_value_ref(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    place: &TirPat,
  ) -> Port {
    let (value, space) = self.distill_pat_place(stage, place);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Ref(wire.pos, value, space));
    wire.neg
  }

  pub(crate) fn distill_pat_place_ref(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    place: &TirPat,
  ) -> (Port, Port) {
    let (value_0, value_1) = self.distill_pat_place(stage, place);
    let ref_in = stage.new_wire(span, ty);
    let ref_out = stage.new_wire(span, ty);
    let value_2 = stage.new_wire(span, place.ty);
    stage.steps.push(Step::Ref(ref_in.pos, value_0, value_2.pos));
    stage.steps.push(Step::Ref(ref_out.neg, value_1, value_2.neg));
    (ref_in.neg, ref_out.pos)
  }

  pub(crate) fn distill_pat_place_deref(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    reference: &TirPat,
  ) -> (Port, Port) {
    let reference = self.distill_pat_value(stage, reference);
    let value = stage.new_wire(span, ty);
    let space = stage.new_wire(span, ty);
    stage.steps.push(Step::Ref(reference, value.pos, space.neg));
    (value.neg, space.pos)
  }
}

impl<'core> Matcher<'core, '_, '_> {
  pub(crate) fn match_ref<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    mut vars: IdxVec<VarId, MatchVar>,
    mut rows: Vec<Row<'p>>,
    var_id: VarId,
    inner_ty: Type,
  ) {
    let (inner_var, inner_local) = self.new_var(stage, &mut vars, MatchVarForm::Place, inner_ty);
    let value = self.borrow_var(stage, &mut vars, var_id, MatchVarKind::Ref(inner_local));
    let (a, b) = stage.local_read_write(inner_local, self.span, inner_ty);
    stage.steps.push(Step::Ref(value, b, a));
    self.eliminate_col(&mut rows, var_id, |pat| match &*pat.kind {
      TirPatKind::Ref(inner) => Some([(inner_var, inner)]),
      _ => unreachable!(),
    });
    self.distill_rows(layer, stage, vars, rows);
  }
}

impl<'core> Emitter<'core, '_> {
  pub(crate) fn emit_ref(&mut self, reference: &Port, value: &Port, space: &Port) {
    let pair = (
      self.emit_port(reference),
      Tree::Comb("ref".into(), Box::new(self.emit_port(value)), Box::new(self.emit_port(space))),
    );
    self.pairs.push(pair)
  }
}
