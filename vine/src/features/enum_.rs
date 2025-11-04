use std::mem::take;

use ivy::ast::Tree;
use vine_util::{idx::IdxVec, parser::Parser};

use crate::{
  components::{
    charter::Charter,
    distiller::Distiller,
    emitter::Emitter,
    lexer::Token,
    matcher::{MatchVar, MatchVarForm, MatchVarKind, Matcher, Row, VarId},
    parser::{VineParser, BRACE_COMMA},
    resolver::Resolver,
  },
  structures::{
    ast::{EnumItem, Expr, Pat, Path, Span, Variant},
    chart::{
      DefId, DefPatternKind, DefTypeKind, DefValueKind, EnumDef, EnumId, EnumVariant, GenericsId,
      VariantId,
    },
    diag::Diag,
    signatures::EnumSig,
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::{Type, TypeCtx, TypeKind},
    vir::{Header, Interface, InterfaceKind, Layer, Port, Stage, Step, Transfer},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl VineParser<'_> {
  pub(crate) fn parse_enum_item(&mut self) -> Result<EnumItem, Diag> {
    self.expect(Token::Enum)?;
    let flex = self.parse_flex()?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let variants = self.parse_delimited(BRACE_COMMA, Self::parse_variant)?;
    Ok(EnumItem { flex, name, generics, variants })
  }

  fn parse_variant(&mut self) -> Result<Variant, Diag> {
    let name = self.parse_ident()?;
    let data = if self.eat(Token::OpenParen)? {
      let ty = self.parse_ty()?;
      self.eat(Token::CloseParen)?;
      Some(ty)
    } else {
      None
    };
    Ok(Variant { name, data })
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_enum_item(&self, e: &EnumItem) -> Doc<'src> {
    Doc::concat([
      Doc("enum"),
      self.fmt_flex(e.flex),
      Doc(" "),
      Doc(e.name.clone()),
      self.fmt_generic_params(&e.generics),
      Doc(" "),
      Doc::brace_comma_multiline(e.variants.iter().map(|v| {
        Doc::concat([
          Doc(v.name.clone()),
          if let Some(data) = &v.data { Doc::paren(self.fmt_ty(data)) } else { Doc("") },
        ])
      })),
    ])
  }
}

impl Charter<'_> {
  pub(crate) fn chart_enum(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: DefId,
    member_vis: DefId,
    enum_item: EnumItem,
  ) -> DefId {
    let def = self.chart_child(parent, enum_item.name.clone(), member_vis, true);
    let generics = self.chart_generics(def, parent_generics, enum_item.generics, false);
    let enum_id = self.chart.enums.next_index();
    let variants =
      IdxVec::from_iter(enum_item.variants.into_iter().enumerate().map(|(id, variant)| {
        let variant_id = VariantId(id);
        let def = self.chart_child(def, variant.name.clone(), vis, true);
        self.define_value(span, def, vis, DefValueKind::Enum(enum_id, variant_id));
        self.define_pattern(span, def, vis, DefPatternKind::Enum(enum_id, variant_id));
        EnumVariant { span, def, name: variant.name, data: variant.data }
      }));
    let enum_def = EnumDef { span, def, name: enum_item.name, generics, variants };
    self.chart.enums.push_to(enum_id, enum_def);
    let ty_kind = DefTypeKind::Enum(enum_id);
    self.define_type(span, def, vis, ty_kind);
    self.chart_flex_impls(def, generics, span, vis, member_vis, ty_kind, enum_item.flex);
    def
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_enum_sig(&mut self, enum_id: EnumId) {
    let enum_def = &self.chart.enums[enum_id];
    self.initialize(enum_def.def, enum_def.generics);
    let variant_data = enum_def
      .variants
      .values()
      .map(|variant| variant.data.as_ref().map(|ty| self.resolve_ty(ty, false)))
      .collect();
    let types = take(&mut self.types);
    self.sigs.enums.push_to(enum_id, TypeCtx { types, inner: EnumSig { variant_data } });
  }

  pub(crate) fn resolve_expr_path_enum(
    &mut self,
    span: Span,
    path: &Path,
    enum_id: EnumId,
    variant_id: VariantId,
    args: &Option<Vec<Expr>>,
  ) -> Result<TirExpr, Diag> {
    let (type_params, _) = self.resolve_generics(path, self.chart.enums[enum_id].generics, true);
    let data_ty =
      self.types.import_with(&self.sigs.enums[enum_id], Some(&type_params), |t, sig| {
        Some(t.transfer(&sig.variant_data[variant_id]?))
      });
    let data = args.as_deref().map(|args| {
      if let [data] = args {
        self.resolve_expr(data)
      } else {
        self.resolve_expr_tuple(span, args).unwrap_or_else(|diag| self.error_expr(span, diag))
      }
    });
    let data = match data_ty {
      Some(data_ty) => Some(match data {
        Some(data) => {
          self.expect_type(data.span, data.ty, data_ty);
          data
        }
        None => self.error_expr(span, Diag::ExpectedDataExpr { span }),
      }),
      None => {
        if data.is_some() {
          self.diags.report(Diag::EnumVariantNoData { span });
        }
        None
      }
    };
    let ty = self.types.new(TypeKind::Enum(enum_id, type_params));
    Ok(TirExpr::new(span, ty, TirExprKind::Enum(enum_id, variant_id, data)))
  }

  pub(crate) fn resolve_pat_path_enum(
    &mut self,
    span: Span,
    path: &Path,
    data: &Option<Vec<Pat>>,
    enum_id: EnumId,
    variant: VariantId,
  ) -> Result<TirPat, Diag> {
    let data = data.as_ref().map(|args| {
      if let [data] = &**args {
        self.resolve_pat(data)
      } else {
        self.resolve_pat_tuple(span, args).unwrap_or_else(|diag| self.error_pat(span, diag))
      }
    });
    let (type_params, _) = self.resolve_generics(path, self.chart.enums[enum_id].generics, true);
    let data_ty =
      self.types.import_with(&self.sigs.enums[enum_id], Some(&type_params), |t, sig| {
        Some(t.transfer(&sig.variant_data[variant]?))
      });
    let data = match data_ty {
      Some(data_ty) => Some(match data {
        Some(data) => {
          self.expect_type(span, data.ty, data_ty);
          data
        }
        None => self.error_pat(span, Diag::ExpectedDataSubpattern { span }),
      }),
      None => {
        if data.is_some() {
          Err(Diag::EnumVariantNoData { span })?
        }
        None
      }
    };
    let ty = self.types.new(TypeKind::Enum(enum_id, type_params));
    Ok(TirPat::new(span, ty, TirPatKind::Enum(enum_id, variant, data)))
  }

  pub(crate) fn resolve_pat_sig_path_enum(&mut self, path: &Path, enum_id: EnumId) -> Type {
    let (type_params, _) = self.resolve_generics(path, self.chart.enums[enum_id].generics, false);
    self.types.new(TypeKind::Enum(enum_id, type_params))
  }

  pub(crate) fn resolve_ty_path_enum(
    &mut self,
    path: &Path,
    inference: bool,
    enum_id: EnumId,
  ) -> Type {
    let generics_id = self.chart.enums[enum_id].generics;
    let (type_params, _) = self.resolve_generics(path, generics_id, inference);
    self.types.new(TypeKind::Enum(enum_id, type_params))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_enum(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    enum_id: EnumId,
    variant_id: VariantId,
    data: &Option<TirExpr>,
  ) -> Port {
    let data = data.as_ref().map(|e| self.distill_expr_value(stage, e));
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Enum(enum_id, variant_id, wire.neg, data));
    wire.pos
  }
}

impl Matcher<'_, '_> {
  pub(crate) fn match_enum<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    vars: IdxVec<VarId, MatchVar>,
    rows: Vec<Row<'p>>,
    var_id: VarId,
    form: MatchVarForm,
    enum_id: EnumId,
    sig: EnumSig,
  ) {
    let interface = self.distiller.interfaces.push(None);

    let stages = Vec::from_iter(sig.variant_data.iter().map(|(variant_id, &content_ty)| {
      let mut vars = vars.clone();
      let mut rows = rows.clone();

      let mut stage = self.distiller.new_stage(layer, self.span, interface);

      let content = content_ty.map(|ty| {
        let (var, local) = self.new_var(&mut stage, &mut vars, form, ty);
        (ty, var, local)
      });

      let new_var = MatchVarKind::Enum(enum_id, variant_id, content.map(|(_, var, _)| var));
      self.restore_var(&mut stage, &mut vars, var_id, new_var);
      stage.header = Header::Match(
        content.map(|(ty, _, local)| stage.local_barrier_write(local, self.span, ty)),
      );

      self.eliminate_col(&mut rows, var_id, |pat| match &*pat.kind {
        TirPatKind::Enum(_, pat_variant_id, content_pat) => (pat_variant_id == &variant_id)
          .then(|| content.map(|(_, var, _)| var).into_iter().zip(content_pat)),
        _ => unreachable!(),
      });

      self.distill_rows(layer, &mut stage, vars, rows);

      self.distiller.finish_stage(stage)
    }));

    self.distiller.interfaces[interface] =
      Some(Interface::new(interface, layer.id, InterfaceKind::Match(enum_id, stages)));

    let value = self.take_var(stage, &vars, &vars[var_id]);
    stage.transfer = Some(Transfer { interface, data: Some(value) });
  }
}

impl Emitter<'_> {
  pub(crate) fn emit_enum(
    &mut self,
    enum_id: EnumId,
    variant_id: VariantId,
    port: &Port,
    fields: &Option<Port>,
  ) {
    let enum_def = &self.chart.enums[enum_id];
    let wire = self.new_wire();
    let mut fields = Tree::n_ary("enum", fields.iter().map(|p| self.emit_port(p)).chain([wire.0]));
    let enum_ = Tree::n_ary(
      "enum",
      (0..enum_def.variants.len())
        .map(|i| if variant_id.0 == i { take(&mut fields) } else { Tree::Erase })
        .chain([wire.1]),
    );
    let pair = (self.emit_port(port), enum_);
    self.pairs.push(pair);
  }
}
