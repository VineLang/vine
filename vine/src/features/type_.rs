use vine_util::parser::Parse;

use crate::{
  components::{
    charter::{ChartedItem, Charter},
    finder::Finder,
    lexer::Token,
    parser::Parser,
    resolver::Resolver,
    synthesizer::SyntheticImpl,
  },
  structures::{
    ast::{ItemKind, Path, Span, TypeItem},
    chart::{
      DefId, DefTypeKind, GenericsId, OpaqueTypeDef, OpaqueTypeId, TypeAliasDef, TypeAliasId, VisId,
    },
    diag::Diag,
    signatures::{TypeAliasSig, TypeAliasState},
    tir::TirImpl,
    types::{Inverted, Type, TypeCtx, TypeKind, Types},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_type_item(&mut self) -> Result<(Span, ItemKind), Diag> {
    self.expect(Token::Type)?;
    let name_span = self.span();
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let ty = self.eat_then(Token::Eq, Self::parse_ty)?;
    self.eat(Token::Semi)?;
    Ok((name_span, ItemKind::Type(TypeItem { name, generics, ty })))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_type_item(&self, t: &TypeItem) -> Doc<'src> {
    Doc::concat([
      Doc("type "),
      Doc(t.name.clone()),
      self.fmt_generic_params(&t.generics),
      match &t.ty {
        Some(ty) => Doc::concat([Doc(" = "), self.fmt_ty(ty)]),
        None => Doc(""),
      },
      Doc(";"),
    ])
  }
}

impl Charter<'_> {
  pub(crate) fn chart_type(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: VisId,
    member_vis: VisId,
    type_item: TypeItem,
  ) -> ChartedItem {
    let def = self.chart_child(parent, span, type_item.name.clone(), member_vis, true);
    let generics = self.chart_generics(def, parent_generics, type_item.generics, false);
    let name = type_item.name;
    match type_item.ty {
      Some(ty) => {
        let alias_id = self.chart.type_aliases.push(TypeAliasDef { span, def, name, generics, ty });
        self.define_type(span, def, vis, DefTypeKind::Alias(alias_id));
        ChartedItem::TypeAlias(def, alias_id)
      }
      None => {
        let opaque_id = self.chart.opaque_types.push(OpaqueTypeDef { span, def, name, generics });
        self.define_type(span, def, vis, DefTypeKind::Opaque(opaque_id));
        ChartedItem::OpaqueType(def, opaque_id)
      }
    }
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_type_alias(&mut self, alias_id: TypeAliasId) {
    match self.sigs.type_aliases.get(alias_id) {
      Some(TypeAliasState::Resolved(_)) => {}
      Some(TypeAliasState::Resolving) => {
        // Cycle detected
        let alias_def = &self.chart.type_aliases[alias_id];
        let slot = self.sigs.type_aliases.get_or_extend(alias_id);
        let error_type =
          self.types.error(self.diags.error(Diag::RecursiveTypeAlias { span: alias_def.ty.span }));
        *slot = TypeAliasState::Resolved(
          self.types.export(|t| TypeAliasSig { ty: t.transfer(&error_type) }),
        );
      }
      _ => {
        let alias_def = &self.chart.type_aliases[alias_id];
        let slot = self.sigs.type_aliases.get_or_extend(alias_id);
        *slot = TypeAliasState::Resolving;

        self.initialize(alias_def.def, alias_def.generics);
        let ty = self.resolve_ty(&alias_def.ty, false);

        let hover = format!(
          "type {}{} = {};",
          alias_def.name,
          self.show_generics(self.cur_generics, false),
          self.types.show(self.chart, ty),
        );
        self.annotations.record_signature(alias_def.span, hover);

        let slot = self.sigs.type_aliases.get_or_extend(alias_id);
        *slot =
          TypeAliasState::Resolved(self.types.export(|t| TypeAliasSig { ty: t.transfer(&ty) }));
      }
    }
  }

  pub(crate) fn resolve_opaque_type(&mut self, opaque_id: OpaqueTypeId) {
    let opaque_def = &self.chart.opaque_types[opaque_id];
    let hover =
      format!("type {}{};", opaque_def.name, self.show_generics(opaque_def.generics, false),);
    self.annotations.record_signature(opaque_def.span, hover);
  }

  pub(crate) fn resolve_ty_path_alias(
    &mut self,
    path: &Path,
    inference: bool,
    type_alias_id: TypeAliasId,
  ) -> Type {
    let generics_id = self.chart.type_aliases[type_alias_id].generics;
    let (type_params, _) = self.resolve_generics(path, generics_id, inference);
    Resolver::new(
      self.chart,
      self.sigs,
      self.diags,
      self.resolutions,
      self.annotations,
      self.fragments,
      self.finder_cache,
    )
    .resolve_type_alias(type_alias_id);
    let resolved_sig = match &self.sigs.type_aliases[type_alias_id] {
      TypeAliasState::Resolved(sig) => sig,
      _ => panic!("Type alias should be resolved at this point"),
    };
    self.types.import(resolved_sig, Some(&type_params)).ty
  }

  pub(crate) fn resolve_ty_path_opaque(
    &mut self,
    path: &Path,
    inference: bool,
    opaque_id: OpaqueTypeId,
  ) -> Type {
    let generics_id = self.chart.opaque_types[opaque_id].generics;
    let (type_params, _) = self.resolve_generics(path, generics_id, inference);
    self.types.new(TypeKind::Opaque(opaque_id, type_params))
  }
}

impl Finder<'_> {
  pub(crate) fn find_auto_impls_opaque(
    &mut self,
    type_params: &[Type],
    types: &Types,
    found: &mut Vec<TypeCtx<TirImpl>>,
  ) {
    if let Some((Inverted(false), TypeKind::Opaque(opaque_type_id, _))) = types.kind(type_params[0])
    {
      let opaque_def_id = self.chart.opaque_types[*opaque_type_id].def;

      if self.chart.visible(VisId::Def(opaque_def_id), self.source) {
        found.push(TypeCtx {
          types: types.clone(),
          inner: TirImpl::Synthetic(SyntheticImpl::Opaque(*opaque_type_id)),
        });
      }
    }
  }
}
