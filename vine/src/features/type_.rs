use vine_util::parser::Parser;

use crate::{
  components::{charter::Charter, lexer::Token, parser::VineParser, resolver::Resolver},
  structures::{
    ast::{Path, Span, TypeItem},
    chart::{DefId, DefTypeKind, OpaqueTypeDef, OpaqueTypeId, TypeAliasDef, TypeAliasId},
    diag::Diag,
    signatures::TypeAliasSig,
    types::{Type, TypeKind},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_type_item(&mut self) -> Result<TypeItem<'core>, Diag<'core>> {
    self.expect(Token::Type)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let ty = self.eat(Token::Eq)?.then(|| self.parse_ty()).transpose()?;
    self.eat(Token::Semi)?;
    Ok(TypeItem { name, generics, ty })
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_type_item(&self, t: &TypeItem<'core>) -> Doc<'src> {
    Doc::concat([
      Doc("type "),
      Doc(t.name),
      self.fmt_generic_params(&t.generics),
      match &t.ty {
        Some(ty) => Doc::concat([Doc(" = "), self.fmt_ty(ty)]),
        None => Doc(""),
      },
      Doc(";"),
    ])
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_type(
    &mut self,
    parent: DefId,
    span: Span,
    vis: DefId,
    member_vis: DefId,
    type_item: TypeItem<'core>,
  ) -> DefId {
    let def = self.chart_child(parent, type_item.name, member_vis, true);
    let generics = self.chart_generics(def, type_item.generics, false);
    let kind = match type_item.ty {
      Some(ty) => {
        let alias_id = self.chart.type_aliases.push(TypeAliasDef { span, def, generics, ty });
        DefTypeKind::Alias(alias_id)
      }
      None => {
        let opaque_id =
          self.chart.opaque_types.push(OpaqueTypeDef { span, def, generics, name: type_item.name });
        DefTypeKind::Opaque(opaque_id)
      }
    };
    self.define_type(span, def, vis, kind);
    def
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_type_alias(&mut self, alias_id: TypeAliasId) {
    if let Some(Some(_)) = self.sigs.type_aliases.get(alias_id) {
      return;
    }
    let alias_def = &self.chart.type_aliases[alias_id];
    self.initialize(alias_def.def, alias_def.generics);
    let ty = self.resolve_ty(&alias_def.ty, false);
    let slot = self.sigs.type_aliases.get_or_extend(alias_id);
    *slot = Some(self.types.export(|t| TypeAliasSig { ty: t.transfer(&ty) }));
  }

  pub(crate) fn resolve_ty_path_alias(
    &mut self,
    path: &Path<'core>,
    inference: bool,
    type_alias_id: TypeAliasId,
  ) -> Type {
    let generics_id = self.chart.type_aliases[type_alias_id].generics;
    let (type_params, _) = self.resolve_generics(path, generics_id, inference);
    Resolver::new(self.core, self.chart, self.sigs, self.resolutions, self.fragments)
      .resolve_type_alias(type_alias_id);
    self
      .types
      .import(self.sigs.type_aliases[type_alias_id].as_ref().unwrap(), Some(&type_params))
      .ty
  }

  pub(crate) fn resolve_ty_path_opaque(
    &mut self,
    path: &Path<'core>,
    inference: bool,
    opaque_id: OpaqueTypeId,
  ) -> Type {
    let generics_id = self.chart.opaque_types[opaque_id].generics;
    let (type_params, _) = self.resolve_generics(path, generics_id, inference);
    self.types.new(TypeKind::Opaque(opaque_id, type_params))
  }
}
