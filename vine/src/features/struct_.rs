use std::mem::take;

use vine_util::{idx::IdxVec, parser::Parser};

use crate::{
  components::{
    charter::Charter,
    distiller::{Distiller, Poly},
    emitter::Emitter,
    lexer::Token,
    matcher::{MatchVar, MatchVarForm, MatchVarKind, Matcher, Row, VarId},
    parser::VineParser,
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, Pat, Path, Span, StructItem},
    chart::{DefId, DefPatternKind, DefTypeKind, DefValueKind, GenericsId, StructDef, StructId},
    diag::Diag,
    signatures::StructSig,
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::{Type, TypeCtx, TypeKind},
    vir::{Layer, Port, Stage, Step},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_struct_item(&mut self) -> Result<StructItem<'core>, Diag<'core>> {
    self.expect(Token::Struct)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::OpenParen)?;
    let data_vis = self.parse_vis()?;
    let data = self.parse_ty()?;
    self.expect(Token::CloseParen)?;
    self.eat(Token::Semi)?;
    Ok(StructItem { name, generics, data_vis, data })
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_struct_item(&self, s: &StructItem<'core>) -> Doc<'src> {
    Doc::concat([
      Doc("struct "),
      Doc(s.name),
      self.fmt_generic_params(&s.generics),
      Doc::paren(Doc::concat([self.fmt_vis(&s.data_vis), self.fmt_ty(&s.data)])),
      Doc(";"),
    ])
  }

  pub(crate) fn fmt_expr_unwrap(&self, expr: &Expr<'core>) -> Doc<'src> {
    Doc::concat([self.fmt_expr(expr), Doc("!")])
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_struct(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: DefId,
    member_vis: DefId,
    struct_item: StructItem<'core>,
  ) -> DefId {
    let def = self.chart_child(parent, struct_item.name, member_vis, true);
    let generics = self.chart_generics(def, parent_generics, struct_item.generics, false);
    let data_vis = self.resolve_vis(parent, struct_item.data_vis);
    let struct_id = self.chart.structs.push(StructDef {
      span,
      def,
      generics,
      name: struct_item.name,
      data_vis,
      data: struct_item.data,
    });
    self.define_value(span, def, data_vis, DefValueKind::Struct(struct_id));
    self.define_pattern(span, def, data_vis, DefPatternKind::Struct(struct_id));
    self.define_type(span, def, vis, DefTypeKind::Struct(struct_id));
    def
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_struct_sig(&mut self, struct_id: StructId) {
    let struct_def = &self.chart.structs[struct_id];
    self.initialize(struct_def.def, struct_def.generics);
    let data = self.resolve_ty(&struct_def.data, false);
    let types = take(&mut self.types);
    self.sigs.structs.push_to(struct_id, TypeCtx { types, inner: StructSig { data } });
  }

  pub(crate) fn resolve_expr_path_struct(
    &mut self,
    expr: &Expr<'core>,
    span: Span,
    path: &Path<'core>,
    struct_id: StructId,
    args: &Option<Vec<Expr<'core>>>,
  ) -> Result<TirExpr, Diag<'core>> {
    let (type_params, _) =
      self.resolve_generics(path, self.chart.structs[struct_id].generics, true);
    let data_ty = self.types.import(&self.sigs.structs[struct_id], Some(&type_params)).data;
    let data = if let Some(args) = args {
      if let [data] = &**args {
        self.resolve_expr(data)
      } else {
        self.resolve_expr_tuple(span, args).unwrap_or_else(|diag| self.error_expr(span, diag))
      }
    } else {
      self.error_expr(span, Diag::ExpectedDataExpr { span })
    };
    if self.types.unify(data.ty, data_ty).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
        span: expr.span,
        expected: self.types.show(self.chart, data_ty),
        found: self.types.show(self.chart, data.ty),
      });
    }
    let ty = self.types.new(TypeKind::Struct(struct_id, type_params));
    Ok(TirExpr::new(span, ty, TirExprKind::Struct(struct_id, data)))
  }

  pub(crate) fn resolve_pat_path_struct(
    &mut self,
    span: Span,
    path: &Path<'core>,
    data: &Option<Vec<Pat<'core>>>,
    struct_id: StructId,
  ) -> Result<TirPat, Diag<'core>> {
    let data = match data {
      Some(args) => {
        if let [data] = &**args {
          self.resolve_pat(data)
        } else {
          self.resolve_pat_tuple(span, args).unwrap_or_else(|diag| self.error_pat(span, diag))
        }
      }
      None => self.error_pat(span, Diag::ExpectedDataSubpattern { span }),
    };
    let (type_params, _) =
      self.resolve_generics(path, self.chart.structs[struct_id].generics, true);
    let data_ty = self.types.import(&self.sigs.structs[struct_id], Some(&type_params)).data;
    self.expect_type(data.span, data.ty, data_ty);
    let ty = self.types.new(TypeKind::Struct(struct_id, type_params));
    Ok(TirPat::new(span, ty, TirPatKind::Struct(struct_id, data)))
  }

  pub(crate) fn resolve_pat_sig_path_struct(
    &mut self,
    path: &Path<'core>,
    struct_id: StructId,
  ) -> Type {
    let (type_params, _) =
      self.resolve_generics(path, self.chart.structs[struct_id].generics, false);
    self.types.new(TypeKind::Struct(struct_id, type_params))
  }

  pub(crate) fn resolve_ty_path_struct(
    &mut self,
    path: &Path<'core>,
    inference: bool,
    struct_id: StructId,
  ) -> Type {
    let generics_id = self.chart.structs[struct_id].generics;
    let (type_params, _) = self.resolve_generics(path, generics_id, inference);
    self.types.new(TypeKind::Struct(struct_id, type_params))
  }

  pub(crate) fn resolve_expr_unwrap(
    &mut self,
    span: Span,
    inner: &Expr<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let inner = self.resolve_expr(inner);

    let (struct_id, data_ty) = match self.types.force_kind(self.core, inner.ty) {
      (inv, TypeKind::Struct(struct_id, type_params)) => {
        let struct_id = *struct_id;
        let type_params = &type_params.clone();
        (struct_id, self.get_struct_data(span, inner.ty, struct_id, type_params).invert_if(inv))
      }
      _ => Err(Diag::UnwrapNonStruct { span })?,
    };

    Ok(TirExpr::new(span, data_ty, TirExprKind::Unwrap(struct_id, inner)))
  }

  pub(crate) fn get_struct_data(
    &mut self,
    span: Span,
    ty: Type,
    struct_id: StructId,
    type_params: &[Type],
  ) -> Type {
    let vis = self.chart.structs[struct_id].data_vis;
    if !self.chart.visible(vis, self.cur_def) {
      self.core.report(Diag::StructDataInvisible {
        span,
        ty: self.types.show(self.chart, ty),
        vis: self.chart.defs[vis].path,
      });
    }
    self.types.import(&self.sigs.structs[struct_id], Some(type_params)).data
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_expr_value_struct(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirExpr,
  ) -> Port {
    let inner = self.distill_expr_value(stage, inner);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, wire.neg, inner));
    wire.pos
  }

  pub(crate) fn distill_expr_value_unwrap(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirExpr,
  ) -> Port {
    let inner = self.distill_expr_value(stage, inner);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, inner, wire.neg));
    wire.pos
  }

  pub(crate) fn distill_expr_space_struct(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirExpr,
  ) -> Port {
    let inner = self.distill_expr_space(stage, inner);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, wire.pos, inner));
    wire.neg
  }

  pub(crate) fn distill_expr_space_unwrap(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirExpr,
  ) -> Port {
    let inner = self.distill_expr_space(stage, inner);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, inner, wire.pos));
    wire.neg
  }

  pub(crate) fn distill_expr_place_struct(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirExpr,
  ) -> (Port, Port) {
    let (inner_value, inner_space) = self.distill_expr_place(stage, inner);
    let value = stage.new_wire(span, ty);
    let space = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, value.neg, inner_value));
    stage.steps.push(Step::Struct(struct_id, space.pos, inner_space));
    (value.pos, space.neg)
  }

  pub(crate) fn distill_expr_place_unwrap(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirExpr,
  ) -> (Port, Port) {
    let (inner_value, inner_space) = self.distill_expr_place(stage, inner);
    let value = stage.new_wire(span, ty);
    let space = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, inner_value, value.neg));
    stage.steps.push(Step::Struct(struct_id, inner_space, space.pos));
    (value.pos, space.neg)
  }

  pub(crate) fn distill_expr_poly_struct(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirExpr,
  ) -> Poly {
    match self.distill_expr_poly(stage, inner) {
      Poly::Error(err) => Poly::Error(err),
      Poly::Value(inner_value) => {
        let value = stage.new_wire(span, ty);
        stage.steps.push(Step::Struct(struct_id, value.neg, inner_value));
        Poly::Value(value.pos)
      }
      Poly::Place((inner_value, inner_space)) => {
        let value = stage.new_wire(span, ty);
        let space = stage.new_wire(span, ty);
        stage.steps.push(Step::Struct(struct_id, value.neg, inner_value));
        stage.steps.push(Step::Struct(struct_id, space.pos, inner_space));
        Poly::Place((value.pos, space.neg))
      }
      Poly::Space(inner_space) => {
        let space = stage.new_wire(span, ty);
        stage.steps.push(Step::Struct(struct_id, space.pos, inner_space));
        Poly::Space(space.neg)
      }
    }
  }

  pub(crate) fn distill_expr_poly_unwrap(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirExpr,
  ) -> Poly {
    match self.distill_expr_poly(stage, inner) {
      Poly::Error(err) => Poly::Error(err),
      Poly::Value(inner_value) => {
        let value = stage.new_wire(span, ty);
        stage.steps.push(Step::Struct(struct_id, inner_value, value.neg));
        Poly::Value(value.pos)
      }
      Poly::Place((inner_value, inner_space)) => {
        let value = stage.new_wire(span, ty);
        let space = stage.new_wire(span, ty);
        stage.steps.push(Step::Struct(struct_id, inner_value, value.neg));
        stage.steps.push(Step::Struct(struct_id, inner_space, space.pos));
        Poly::Place((value.pos, space.neg))
      }
      Poly::Space(inner_space) => {
        let space = stage.new_wire(span, ty);
        stage.steps.push(Step::Struct(struct_id, inner_space, space.pos));
        Poly::Space(space.neg)
      }
    }
  }

  pub(crate) fn distill_pat_value_struct(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirPat,
  ) -> Port {
    let inner = self.distill_pat_value(stage, inner);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, wire.pos, inner));
    wire.neg
  }

  pub(crate) fn distill_pat_space_struct(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirPat,
  ) -> Port {
    let inner = self.distill_pat_space(stage, inner);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, wire.neg, inner));
    wire.pos
  }

  pub(crate) fn distill_pat_place_struct(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    struct_id: StructId,
    inner: &TirPat,
  ) -> (Port, Port) {
    let (inner_value, inner_space) = self.distill_pat_place(stage, inner);
    let value = stage.new_wire(span, ty);
    let space = stage.new_wire(span, ty);
    stage.steps.push(Step::Struct(struct_id, value.pos, inner_value));
    stage.steps.push(Step::Struct(struct_id, space.neg, inner_space));
    (value.neg, space.pos)
  }
}

impl<'core> Matcher<'core, '_, '_> {
  pub(crate) fn match_struct<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    mut vars: IdxVec<VarId, MatchVar>,
    mut rows: Vec<Row<'p>>,
    var_id: VarId,
    form: MatchVarForm,
    struct_id: StructId,
    sig: StructSig,
  ) {
    let content_ty = sig.data;
    let (content_var, content_local) = self.new_var(stage, &mut vars, form, content_ty);
    let value =
      self.borrow_var(stage, &mut vars, var_id, MatchVarKind::Struct(struct_id, content_var));
    let content = stage.new_wire(self.span, content_ty);
    stage.steps.push(Step::Struct(struct_id, value, content.neg));
    stage.local_barrier_write_to(content_local, content.pos);
    self.eliminate_col(&mut rows, var_id, |pat| match &*pat.kind {
      TirPatKind::Struct(_, inner) => Some([(content_var, inner)]),
      _ => unreachable!(),
    });
    self.distill_rows(layer, stage, vars, rows);
  }
}

impl<'core> Emitter<'core, '_> {
  pub(crate) fn emit_struct(&mut self, _struct_id: StructId, a: &Port, b: &Port) {
    let a = self.emit_port(a);
    let b = self.emit_port(b);
    self.pairs.push((a, b))
  }
}
