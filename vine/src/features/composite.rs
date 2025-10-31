use std::{collections::BTreeMap, mem::replace};

use ivy::ast::Tree;
use vine_util::{idx::IdxVec, parser::Parser};

use crate::{
  components::{
    distiller::{Distiller, Poly},
    emitter::Emitter,
    lexer::Token,
    matcher::{MatchVar, MatchVarForm, MatchVarKind, Matcher, Row, VarId},
    parser::{VineParser, BRACE_COMMA, PAREN_COMMA},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Ident, Key, Pat, PatKind, Span, Ty, TyKind},
    diag::{Diag, ErrorGuaranteed},
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::{Inverted, Type, TypeKind},
    vir::{Layer, Port, Stage, Step},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl VineParser<'_> {
  pub(crate) fn parse_expr_paren(&mut self) -> Result<ExprKind, Diag> {
    self.expect(Token::OpenParen)?;
    if self.eat(Token::CloseParen)? {
      return Ok(ExprKind::Tuple(vec![]));
    }
    let expr = self.parse_expr()?;
    if self.eat(Token::Semi)? {
      let value = expr;
      let space = self.parse_expr()?;
      self.expect(Token::CloseParen)?;
      return Ok(ExprKind::Place(value, space));
    }
    if self.eat(Token::CloseParen)? {
      return Ok(ExprKind::Paren(expr));
    }
    self.expect(Token::Comma)?;
    let mut exprs = vec![expr];
    loop {
      if self.check(Token::CloseParen) {
        break;
      }
      exprs.push(self.parse_expr()?);
      if !self.eat(Token::Comma)? {
        break;
      }
    }
    self.expect(Token::CloseParen)?;
    Ok(ExprKind::Tuple(exprs))
  }

  pub(crate) fn parse_pat_paren(&mut self) -> Result<PatKind, Diag> {
    let mut tuple = false;
    let mut pats = self.parse_delimited(PAREN_COMMA, |self_| {
      let expr = self_.parse_pat()?;
      if self_.check(Token::Comma) {
        tuple = true;
      }
      Ok(expr)
    })?;
    if pats.len() == 1 && !tuple {
      Ok(PatKind::Paren(pats.pop().unwrap()))
    } else {
      Ok(PatKind::Tuple(pats))
    }
  }

  pub(crate) fn parse_ty_paren(&mut self) -> Result<TyKind, Diag> {
    let mut tuple = false;
    let mut types = self.parse_delimited(PAREN_COMMA, |self_| {
      let expr = self_.parse_ty()?;
      if self_.check(Token::Comma) {
        tuple = true;
      }
      Ok(expr)
    })?;
    if types.len() == 1 && !tuple {
      Ok(TyKind::Paren(types.pop().unwrap()))
    } else {
      Ok(TyKind::Tuple(types))
    }
  }

  pub(crate) fn _parse_expr_tuple_field(&mut self, lhs: Expr) -> Result<ExprKind, Diag> {
    let num_span = self.span();
    let num = self.expect(Token::Num)?;
    if let Some((i, j)) = num.split_once(".") {
      let i_span = Span { file: self.file, start: num_span.start, end: num_span.start + i.len() };
      let j_span = Span { file: self.file, start: num_span.end - j.len(), end: num_span.end };
      let i = self.parse_u32_like(i, |_| Diag::InvalidNum { span: i_span })? as usize;
      let j = self.parse_u32_like(j, |_| Diag::InvalidNum { span: j_span })? as usize;
      Ok(ExprKind::TupleField(
        Expr {
          span: Span { file: self.file, start: lhs.span.start, end: i_span.end },
          kind: Box::new(ExprKind::TupleField(lhs, i)),
        },
        j,
      ))
    } else {
      let i = self.parse_u32_like(num, |_| Diag::InvalidNum { span: num_span })? as usize;
      Ok(ExprKind::TupleField(lhs, i))
    }
  }

  pub(crate) fn parse_expr_object(&mut self) -> Result<ExprKind, Diag> {
    Ok(ExprKind::Object(self.parse_delimited(BRACE_COMMA, |self_| {
      let key = self_.parse_key()?;
      let value = if self_.eat(Token::Colon)? {
        self_.parse_expr()?
      } else {
        Expr { span: key.span, kind: Box::new(ExprKind::Path(key.clone().into(), None)) }
      };
      Ok((key, value))
    })?))
  }

  pub(crate) fn parse_pat_object(&mut self) -> Result<PatKind, Diag> {
    Ok(PatKind::Object(self.parse_delimited(BRACE_COMMA, |self_| {
      let span = self_.start_span();
      let key = self_.parse_key()?;
      let (pat, ty) = if self_.eat(Token::Colon)? {
        if self_.eat(Token::Colon)? {
          (None, Some(self_.parse_ty()?))
        } else {
          (Some(self_.parse_pat()?), None)
        }
      } else if self_.eat(Token::ColonColon)? {
        (None, Some(self_.parse_ty()?))
      } else {
        (None, None)
      };
      let span = self_.end_span(span);
      let mut pat = pat.unwrap_or_else(|| Pat {
        span: key.span,
        kind: Box::new(PatKind::Path(key.clone().into(), None)),
      });
      if let Some(ty) = ty {
        pat = Pat { span, kind: Box::new(PatKind::Annotation(pat, ty)) };
      }
      Ok((key, pat))
    })?))
  }

  pub(crate) fn parse_ty_object(&mut self) -> Result<TyKind, Diag> {
    Ok(TyKind::Object(self.parse_delimited(BRACE_COMMA, |self_| {
      let key = self_.parse_key()?;
      self_.expect(Token::Colon)?;
      let value = self_.parse_ty()?;
      Ok((key, value))
    })?))
  }

  fn parse_key(&mut self) -> Result<Key, Diag> {
    let span = self.start_span();
    let ident = self.parse_ident()?;
    let span = self.end_span(span);
    Ok(Key { span, ident })
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_tuple(&self, t: &[Expr]) -> Doc<'src> {
    Doc::tuple(t.iter().map(|x| self.fmt_expr(x)))
  }

  pub(crate) fn fmt_expr_tuple_field(&self, expr: &Expr, index: usize) -> Doc<'src> {
    Doc::concat([self.fmt_expr(expr), Doc("."), Doc(format!("{index}"))])
  }

  pub(crate) fn fmt_pat_tuple(&self, t: &[Pat]) -> Doc<'src> {
    Doc::tuple(t.iter().map(|x| self.fmt_pat(x)))
  }

  pub(crate) fn fmt_ty_tuple(&self, elements: &[Ty]) -> Doc<'src> {
    Doc::tuple(elements.iter().map(|x| self.fmt_ty(x)))
  }

  pub(crate) fn fmt_expr_object(&self, entries: &[(Key, Expr)]) -> Doc<'src> {
    Doc::brace_comma_space(entries.iter().map(|(key, expr)| {
      if let ExprKind::Path(path, None) = &*expr.kind {
        if let Some(i) = path.as_ident() {
          if key.ident == i {
            return Doc(key.ident.clone());
          }
        }
      }
      Doc::concat([Doc(key.ident.clone()), Doc(": "), self.fmt_expr(expr)])
    }))
  }

  pub(crate) fn fmt_expr_object_field(&self, expr: &Expr, key: &Key) -> Doc<'src> {
    Doc::concat([self.fmt_expr(expr), Doc("."), Doc(key.ident.clone())])
  }

  pub(crate) fn fmt_pat_object(&self, entries: &[(Key, Pat)]) -> Doc<'src> {
    Doc::brace_comma_space(entries.iter().map(|(key, pat)| {
      let (pat, ty) = match &*pat.kind {
        PatKind::Annotation(p, t) => (p, Some(t)),
        _ => (pat, None),
      };
      let pat = if let PatKind::Path(path, None) = &*pat.kind {
        if let Some(i) = path.as_ident() {
          if key.ident == i {
            None
          } else {
            Some(pat)
          }
        } else {
          Some(pat)
        }
      } else {
        Some(pat)
      };
      match (pat, ty) {
        (None, None) => Doc(key.ident.clone()),
        (Some(pat), None) => Doc::concat([Doc(key.ident.clone()), Doc(": "), self.fmt_pat(pat)]),
        (None, Some(ty)) => Doc::concat([Doc(key.ident.clone()), Doc(":: "), self.fmt_ty(ty)]),
        (Some(pat), Some(ty)) => Doc::concat([
          Doc(key.ident.clone()),
          Doc(": "),
          self.fmt_pat(pat),
          Doc(": "),
          self.fmt_ty(ty),
        ]),
      }
    }))
  }

  pub(crate) fn fmt_ty_object(&self, entries: &[(Key, Ty)]) -> Doc<'src> {
    Doc::brace_comma_space(
      entries.iter().map(|(k, t)| Doc::concat([Doc(k.ident.clone()), Doc(": "), self.fmt_ty(t)])),
    )
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_tuple(
    &mut self,
    span: Span,
    elements: &[Expr],
  ) -> Result<TirExpr, Diag> {
    let elements = elements.iter().map(|x| self.resolve_expr(x)).collect::<Vec<_>>();
    let ty = self.types.new(TypeKind::Tuple(elements.iter().map(|x| x.ty).collect()));
    Ok(TirExpr::new(span, ty, TirExprKind::Composite(elements)))
  }

  pub(crate) fn resolve_pat_tuple(&mut self, span: Span, elements: &[Pat]) -> Result<TirPat, Diag> {
    let elements = Vec::from_iter(elements.iter().map(|element| self.resolve_pat(element)));
    let ty = self.types.new(TypeKind::Tuple(elements.iter().map(|x| x.ty).collect()));
    Ok(TirPat::new(span, ty, TirPatKind::Composite(elements)))
  }

  pub(crate) fn resolve_pat_sig_tuple(&mut self, elements: &[Pat], inference: bool) -> Type {
    let elements =
      elements.iter().map(|element| self.resolve_pat_sig(element, inference)).collect();
    self.types.new(TypeKind::Tuple(elements))
  }

  pub(crate) fn resolve_ty_tuple(&mut self, tys: &[Ty], inference: bool) -> Type {
    let tys = tys.iter().map(|arg| self.resolve_ty(arg, inference)).collect();
    self.types.new(TypeKind::Tuple(tys))
  }

  pub(crate) fn resolve_expr_object(
    &mut self,
    span: Span,
    entries: &[(Key, Expr)],
  ) -> Result<TirExpr, Diag> {
    let object = self._build_object(entries, Self::resolve_expr)?;
    let ty =
      self.types.new(TypeKind::Object(object.iter().map(|(i, x)| (i.clone(), x.ty)).collect()));
    Ok(TirExpr::new(span, ty, TirExprKind::Composite(object.into_values().collect())))
  }

  pub(crate) fn resolve_pat_object(
    &mut self,
    span: Span,
    entries: &[(Key, Pat)],
  ) -> Result<TirPat, Diag> {
    let object = self._build_object(entries, |self_, pat| self_.resolve_pat(pat))?;
    let ty =
      self.types.new(TypeKind::Object(object.iter().map(|(i, x)| (i.clone(), x.ty)).collect()));
    Ok(TirPat::new(span, ty, TirPatKind::Composite(object.into_values().collect())))
  }

  pub(crate) fn resolve_pat_sig_object(&mut self, entries: &[(Key, Pat)], inference: bool) -> Type {
    self._build_object_type(entries, |self_, pat| self_.resolve_pat_sig(pat, inference))
  }

  pub(crate) fn resolve_ty_object(&mut self, entries: &[(Key, Ty)], inference: bool) -> Type {
    self._build_object_type(entries, |self_, t| self_.resolve_ty(t, inference))
  }

  fn _build_object<T, U>(
    &mut self,
    entries: &[(Key, T)],
    mut f: impl FnMut(&mut Self, &T) -> U,
  ) -> Result<BTreeMap<Ident, U>, ErrorGuaranteed> {
    let mut object = BTreeMap::new();
    let mut duplicate = Ok(());
    for (key, value) in entries {
      let old = object.insert(key.ident.clone(), f(self, value));
      if old.is_some() {
        duplicate = Err(self.core.report(Diag::DuplicateKey { span: key.span }));
      }
    }
    duplicate?;
    Ok(object)
  }

  fn _build_object_type<T>(
    &mut self,
    entries: &[(Key, T)],
    mut f: impl FnMut(&mut Self, &T) -> Type,
  ) -> Type {
    let mut fields = BTreeMap::new();
    let mut duplicate = Ok(());
    for (key, value) in entries {
      let old = fields.insert(key.ident.clone(), f(self, value));
      if old.is_some() {
        duplicate = Err(self.core.report(Diag::DuplicateKey { span: key.span }));
      }
    }
    if let Err(err) = duplicate {
      self.types.error(err)
    } else {
      self.types.new(TypeKind::Object(fields))
    }
  }

  pub(crate) fn resolve_expr_tuple_field(
    &mut self,
    span: Span,
    tuple: &Expr,
    index: usize,
  ) -> Result<TirExpr, Diag> {
    let tuple = self.resolve_expr(tuple);
    let (ty, fields) = self.tuple_field(span, tuple.ty, index).ok_or_else(|| {
      Diag::MissingTupleField { span, ty: self.types.show(self.chart, tuple.ty), i: index }
    })?;
    Ok(TirExpr::new(span, ty, TirExprKind::Field(tuple, index, fields)))
  }

  fn tuple_field(&mut self, span: Span, ty: Type, index: usize) -> Option<(Type, Vec<Type>)> {
    match self.types.force_kind(self.core, ty) {
      (inv, TypeKind::Tuple(tuple)) => Some((tuple.get(index)?.invert_if(inv), tuple.clone())),
      (inv, TypeKind::Struct(struct_id, type_params)) => {
        let struct_id = *struct_id;
        let type_params = &type_params.clone();
        let data_ty = self.get_struct_data(span, ty, struct_id, type_params);
        self.tuple_field(span, data_ty.invert_if(inv), index)
      }
      (_, TypeKind::Error(_)) => Some((ty, vec![ty; index + 1])),
      _ => None,
    }
  }

  pub(crate) fn resolve_expr_object_field(
    &mut self,
    span: Span,
    object: &Expr,
    key: &Key,
  ) -> Result<TirExpr, Diag> {
    let object = self.resolve_expr(object);
    let (ty, index, fields) =
      self.object_field(span, object.ty, key.ident.clone()).ok_or_else(|| {
        Diag::MissingObjectField {
          span: key.span,
          ty: self.types.show(self.chart, object.ty),
          key: key.ident.clone(),
        }
      })?;
    Ok(TirExpr::new(span, ty, TirExprKind::Field(object, index, fields)))
  }

  fn object_field(&mut self, span: Span, ty: Type, key: Ident) -> Option<(Type, usize, Vec<Type>)> {
    match self.types.force_kind(self.core, ty) {
      (inv, TypeKind::Object(entries)) => entries
        .iter()
        .enumerate()
        .find(|&(_, (k, _))| *k == key)
        .map(|(i, (_, t))| (t.invert_if(inv), i, entries.values().copied().collect())),
      (inv, TypeKind::Struct(struct_id, type_params)) => {
        let struct_id = *struct_id;
        let type_params = &type_params.clone();
        let data_ty = self.get_struct_data(span, ty, struct_id, type_params);
        self.object_field(span, data_ty.invert_if(inv), key)
      }
      (_, TypeKind::Error(_)) => Some((ty, 0, vec![ty])),
      _ => None,
    }
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_composite(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    elements: &[TirExpr],
  ) -> Port {
    self.distill_vec(stage, span, ty, elements, Self::distill_expr_value, Step::Composite)
  }

  pub(crate) fn distill_expr_space_composite(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    elements: &[TirExpr],
  ) -> Port {
    self.distill_vec(stage, span, ty.inverse(), elements, Self::distill_expr_space, Step::Composite)
  }

  pub(crate) fn distill_expr_place_composite(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    elements: &[TirExpr],
  ) -> (Port, Port) {
    self.distill_vec_pair(stage, span, ty, elements, Self::distill_expr_place, Step::Composite)
  }

  pub(crate) fn distill_expr_poly_composite(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    elements: &[TirExpr],
  ) -> Poly {
    let mut acc = None;
    for element in elements {
      let element = self.distill_expr_poly(stage, element);
      let mut new_acc = acc.unwrap_or_else(|| match element {
        Poly::Error(err) => Poly::Error(err),
        Poly::Value(_) => Poly::Value(vec![]),
        Poly::Place(_) => Poly::Place((vec![], vec![])),
        Poly::Space(_) => Poly::Space(vec![]),
      });
      match (&mut new_acc, element) {
        (_, Poly::Error(err)) => new_acc = Poly::Error(err),
        (Poly::Value(ps), Poly::Value(p)) | (Poly::Space(ps), Poly::Space(p)) => ps.push(p),
        (Poly::Place((ps, qs)), Poly::Place((p, q))) => {
          ps.push(p);
          qs.push(q);
        }
        _ => new_acc = Poly::Error(self.core.report(Diag::AmbiguousPolyformicComposite { span })),
      }
      acc = Some(new_acc);
    }
    match acc.unwrap_or(Poly::Place((vec![], vec![]))) {
      Poly::Error(err) => Poly::Error(err),
      Poly::Value(values) => {
        let value = stage.new_wire(span, ty);
        stage.steps.push(Step::Composite(value.neg, values));
        Poly::Value(value.pos)
      }
      Poly::Place((values, spaces)) => {
        let value = stage.new_wire(span, ty);
        let space = stage.new_wire(span, ty);
        stage.steps.push(Step::Composite(value.neg, values));
        stage.steps.push(Step::Composite(space.pos, spaces));
        Poly::Place((value.pos, space.neg))
      }
      Poly::Space(spaces) => {
        let space = stage.new_wire(span, ty);
        stage.steps.push(Step::Composite(space.pos, spaces));
        Poly::Space(space.neg)
      }
    }
  }

  pub(crate) fn distill_pat_value_composite(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    elements: &[TirPat],
  ) -> Port {
    self.distill_vec(stage, span, ty.inverse(), elements, Self::distill_pat_value, Step::Composite)
  }

  pub(crate) fn distill_pat_space_composite(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    elements: &[TirPat],
  ) -> Port {
    self.distill_vec(stage, span, ty, elements, Self::distill_pat_space, Step::Composite)
  }

  pub(crate) fn distill_pat_place_composite(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    elements: &[TirPat],
  ) -> (Port, Port) {
    self.distill_vec_pair(
      stage,
      span,
      ty.inverse(),
      elements,
      Self::distill_pat_place,
      Step::Composite,
    )
  }

  pub(crate) fn distill_expr_poly_field(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    inner: &TirExpr,
    index: usize,
    fields: &[Type],
  ) -> Poly {
    let inner = self.distill_expr_poly(stage, inner);
    match inner {
      Poly::Error(err) => Poly::Error(err),
      Poly::Value(value) => {
        let wire = stage.new_wire(span, ty);
        let mut neg = Some(wire.neg);
        let ports = Vec::from_iter(fields.iter().enumerate().map(|(i, &ty)| {
          if i == index {
            neg.take().unwrap()
          } else {
            self.drop_space(span, stage, ty)
          }
        }));
        stage.steps.push(Step::Composite(value, ports));
        Poly::Value(wire.pos)
      }
      Poly::Place(place) => {
        let (mut neg, pos) = fields
          .iter()
          .map(|&ty| {
            let wire = stage.new_wire(span, ty);
            (wire.neg, wire.pos)
          })
          .collect::<(Vec<_>, Vec<_>)>();
        let wire = stage.new_wire(span, ty);
        let value = wire.pos;
        let space = replace(&mut neg[index], wire.neg);
        stage.steps.push(Step::Composite(place.0, neg));
        stage.steps.push(Step::Composite(place.1, pos));
        Poly::Place((value, space))
      }
      Poly::Space(_) => Poly::Error(self.core.report(Diag::SpaceField { span })),
    }
  }
}

impl Matcher<'_, '_> {
  pub(crate) fn match_composite<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    mut vars: IdxVec<VarId, MatchVar>,
    mut rows: Vec<Row<'p>>,
    var_id: VarId,
    form: MatchVarForm,
    inv: Inverted,
    element_tys: Vec<Type>,
  ) {
    let element_tys = element_tys.iter().map(|&t| t.invert_if(inv));
    let (element_vars, element_locals): (Vec<_>, Vec<_>) =
      element_tys.clone().map(|t| self.new_var(stage, &mut vars, form, t)).collect();
    let value =
      self.borrow_var(stage, &mut vars, var_id, MatchVarKind::Composite(element_vars.clone()));
    let element_ports = Vec::from_iter(
      element_tys
        .zip(element_locals)
        .map(|(ty, local)| stage.local_barrier_write(local, self.span, ty)),
    );
    stage.steps.push(Step::Composite(value, element_ports));
    self.eliminate_col(&mut rows, var_id, |pat| match &*pat.kind {
      TirPatKind::Composite(pats) => Some(element_vars.iter().copied().zip(pats)),
      _ => unreachable!(),
    });
    self.distill_rows(layer, stage, vars, rows);
  }
}

impl Emitter<'_> {
  pub(crate) fn emit_composite(&mut self, port: &Port, tuple: &[Port]) {
    let pair = (self.emit_port(port), Tree::n_ary("tup", tuple.iter().map(|p| self.emit_port(p))));
    self.pairs.push(pair)
  }
}
