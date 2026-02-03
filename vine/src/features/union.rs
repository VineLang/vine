use std::{collections::HashMap, mem::take};

use vine_util::idx::IdxVec;

use crate::{
  components::{
    charter::{ChartedItem, Charter},
    distiller::Distiller,
    emitter::Emitter,
    resolver::Resolver,
  },
  structures::{
    ast::{EnumItem, Expr, Pat, Path, Span},
    chart::{
      DefId, DefPatternKind, DefTypeKind, DefValueKind, GenericsId, UnionDef, UnionId,
      UnionVariant, VariantId, VisId,
    },
    diag::Diag,
    signatures::UnionSig,
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::{Type, TypeCtx, TypeKind},
    vir::{Port, Stage, Step},
  },
};

impl Charter<'_> {
  pub(crate) fn chart_union(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: VisId,
    member_vis: VisId,
    enum_item: EnumItem,
  ) -> ChartedItem {
    let def = self.chart_child(parent, span, enum_item.name.clone(), member_vis, true);
    let generics = self.chart_generics(def, parent_generics, enum_item.generics, false);
    let union_id = self.chart.unions.next_index();
    let variants =
      IdxVec::from_iter(enum_item.variants.into_iter().enumerate().map(|(id, variant)| {
        let variant_id = VariantId(id);
        let def = self.chart_child(def, span, variant.name.clone(), vis, true);
        self.define_value(span, def, vis, DefValueKind::Union(union_id, variant_id));
        self.define_pattern(span, def, vis, DefPatternKind::Union(union_id, variant_id));
        UnionVariant { span, def, name: variant.name, data: variant.data }
      }));
    let union_def =
      UnionDef { span, def, name: enum_item.name, generics, variants, self_dual: false };
    self.chart.unions.push_to(union_id, union_def);
    let ty_kind = DefTypeKind::Union(union_id);
    self.define_type(span, def, vis, ty_kind);
    self.chart_flex_impls(def, generics, vis, ty_kind, enum_item.flex_span, enum_item.flex);
    ChartedItem::Union(def, union_id)
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_union_sig(&mut self, union_id: UnionId) {
    let union_def = &self.chart.unions[union_id];
    self.initialize(union_def.def, union_def.generics, false);
    let variant_data = IdxVec::from_iter(union_def.variants.values().map(|variant| {
      let mut data = variant.data.iter().map(|t| self.resolve_ty(t, false)).collect::<Vec<_>>();
      if data.len() == 1 { data.pop().unwrap() } else { self.types.new(TypeKind::Tuple(data)) }
    }));
    let variant_lookup =
      HashMap::from_iter(union_def.variants.iter().map(|(id, variant)| (variant.name.clone(), id)));
    let hover = format!(
      "unsafe enum {}{} {{ ... }}",
      union_def.name,
      self.show_generics(self.cur_generics, false),
    );
    self.annotations.record_signature(union_def.span, hover);
    if union_def.self_dual && !variant_data.values().all(|&ty| self.types._self_inverse(ty, true)) {
      self.diags.error(Diag::NotSelfDual { span: union_def.span });
    }
    let types = take(&mut self.types);
    self
      .sigs
      .unions
      .push_to(union_id, TypeCtx { types, inner: UnionSig { variant_data, variant_lookup } });
  }

  pub(crate) fn resolve_expr_path_union(
    &mut self,
    span: Span,
    path: &Path,
    union_id: UnionId,
    variant_id: VariantId,
    args: &Option<Vec<Expr>>,
  ) -> Result<TirExpr, Diag> {
    self.unsafe_(span);
    let (type_params, _) = self.resolve_generics(path, self.chart.unions[union_id].generics, true);
    let data_ty =
      self.types.import_with(&self.sigs.unions[union_id], Some(&type_params), |t, sig| {
        t.transfer(&sig.variant_data[variant_id])
      });
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
      self.diags.error(Diag::ExpectedTypeFound {
        span: data.span,
        expected: self.types.show(self.chart, data_ty),
        found: self.types.show(self.chart, data.ty),
      });
    }
    let ty = self.types.new_union(self.chart, union_id, type_params);
    Ok(TirExpr::new(span, ty, TirExprKind::Rewrap(data)))
  }

  pub(crate) fn resolve_pat_path_union(
    &mut self,
    span: Span,
    path: &Path,
    data: &Option<Vec<Pat>>,
    union_id: UnionId,
    variant: VariantId,
  ) -> Result<TirPat, Diag> {
    self.unsafe_(span);
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
    let (type_params, _) = self.resolve_generics(path, self.chart.unions[union_id].generics, true);
    let data_ty =
      self.types.import_with(&self.sigs.unions[union_id], Some(&type_params), |t, sig| {
        t.transfer(&sig.variant_data[variant])
      });
    self.expect_type(data.span, data.ty, data_ty);
    let ty = self.types.new_union(self.chart, union_id, type_params);
    Ok(TirPat::new(span, ty, TirPatKind::Union(data)))
  }

  pub(crate) fn resolve_pat_sig_path_union(&mut self, path: &Path, union_id: UnionId) -> Type {
    let (type_params, _) = self.resolve_generics(path, self.chart.unions[union_id].generics, false);
    self.types.new_union(self.chart, union_id, type_params)
  }

  pub(crate) fn resolve_ty_path_union(
    &mut self,
    path: &Path,
    inference: bool,
    union_id: UnionId,
  ) -> Type {
    let generics_id = self.chart.unions[union_id].generics;
    let (type_params, _) = self.resolve_generics(path, generics_id, inference);
    self.types.new_union(self.chart, union_id, type_params)
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_pat_value_union(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    inner: &TirPat,
  ) -> Port {
    let inner = self.distill_pat_value(stage, inner);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Rewrap(wire.pos, inner));
    wire.neg
  }

  pub(crate) fn distill_pat_space_union(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    inner: &TirPat,
  ) -> Port {
    let inner = self.distill_pat_space(stage, inner);
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Rewrap(wire.neg, inner));
    wire.pos
  }

  pub(crate) fn distill_pat_place_union(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    inner: &TirPat,
  ) -> (Port, Port) {
    let (inner_value, inner_space) = self.distill_pat_place(stage, inner);
    let value = stage.new_wire(span, ty);
    let space = stage.new_wire(span, ty);
    stage.steps.push(Step::Rewrap(value.pos, inner_value));
    stage.steps.push(Step::Rewrap(space.neg, inner_space));
    (value.neg, space.pos)
  }
}

impl Emitter<'_> {
  pub(crate) fn emit_union(
    &mut self,
    _union_id: UnionId,
    _variant_id: VariantId,
    a: &Port,
    b: &Port,
  ) {
    let a = self.emit_port(a);
    let b = self.emit_port(b);
    self.pairs.push((a, b))
  }
}
