use crate::{
  ast::{GenericPath, Span},
  checker::{Checker, Type},
  diag::Diag,
  resolver::Def,
};

impl<'core> Checker<'core, '_> {
  pub(super) fn typeof_value_def(
    &mut self,
    path: &mut GenericPath<'core>,
  ) -> Result<Type<'core>, Diag<'core>> {
    let span = path.span;
    let def_id = path.path.resolved.unwrap();
    let def = &self.resolver.defs[def_id];
    let Some(value_def) = &def.value_def else {
      return Err(Diag::PathNoValue { span, path: def.canonical.clone() });
    };
    if !self.resolver.visible(value_def.vis, self.cur_def) {
      return Err(Diag::ValueInvisible {
        span,
        path: def.canonical.clone(),
        vis: self.resolver.defs[value_def.vis].canonical.clone(),
      });
    }
    let generic_count = value_def.generics.len();
    Self::check_generic_count(span, def, path, generic_count)?;
    let generics = self.hydrate_generics(path, generic_count, true);
    let def = &self.resolver.defs[def_id];
    Ok(def.value_def.as_ref().unwrap().ty.as_ref().unwrap().instantiate(&generics))
  }

  pub(super) fn typeof_type_def(
    &mut self,
    path: &mut GenericPath<'core>,
    inference: bool,
  ) -> Result<Type<'core>, Diag<'core>> {
    let span = path.span;
    let def_id = path.path.resolved.unwrap();
    let def = &self.resolver.defs[def_id];
    let Some(type_def) = &def.type_def else {
      Err(Diag::PathNoType { span, path: def.canonical.clone() })?
    };
    if !self.resolver.visible(type_def.vis, self.cur_def) {
      return Err(Diag::TypeInvisible {
        span,
        path: def.canonical.clone(),
        vis: self.resolver.defs[type_def.vis].canonical.clone(),
      });
    }
    let generic_count = type_def.generics.len();
    Self::check_generic_count(span, def, path, generic_count)?;
    if !inference && path.generics.is_none() && generic_count != 0 {
      Err(Diag::ItemTypeHole { span })?
    }
    let generics = self.hydrate_generics(path, generic_count, inference);
    let def = &self.resolver.defs[def_id];
    if def.adt_def.is_some() {
      Ok(Type::Adt(def_id, generics))
    } else {
      self.resolve_type_alias(def.id);
      let def = &self.resolver.defs[def_id];
      if let Some(ty) = &def.type_def.as_ref().unwrap().ty {
        Ok(ty.instantiate(&generics))
      } else {
        Err(Diag::RecursiveTypeAlias { span })
      }
    }
  }

  pub(super) fn typeof_variant_def(
    &mut self,
    path: &mut GenericPath<'core>,
    refutable: bool,
    inference: bool,
  ) -> Result<(Type<'core>, Option<Vec<Type<'core>>>), Diag<'core>> {
    let span = path.span;
    let variant_id = path.path.resolved.unwrap();
    let variant = &self.resolver.defs[variant_id];
    let Some(variant_def) = &variant.variant_def else {
      Err(Diag::PathNoPat { span, path: variant.canonical.clone() })?
    };
    if !self.resolver.visible(variant_def.vis, self.cur_def) {
      return Err(Diag::PatInvisible {
        span,
        path: variant.canonical.clone(),
        vis: self.resolver.defs[variant_def.vis].canonical.clone(),
      });
    }
    let adt_id = variant_def.adt;
    let adt = &self.resolver.defs[adt_id];
    let adt_def = adt.adt_def.as_ref().unwrap();
    if !refutable && adt_def.variants.len() > 1 {
      self.core.report(Diag::ExpectedIrrefutablePat { span });
    }
    let generic_count = adt_def.generics.len();
    Self::check_generic_count(span, variant, path, generic_count)?;
    if !inference && path.generics.is_none() && generic_count != 0 {
      Err(Diag::ItemTypeHole { span })?
    }
    let generics = self.hydrate_generics(path, generic_count, inference);
    let field_tys = if inference {
      let variant = &self.resolver.defs[variant_id];
      let variant_def = variant.variant_def.as_ref().unwrap();
      let field_tys = variant_def.field_types.as_ref().unwrap();
      let field_tys = field_tys.iter().map(|t| t.instantiate(&generics)).collect::<Vec<_>>();
      Some(field_tys)
    } else {
      None
    };
    let adt = Type::Adt(adt_id, generics);
    Ok((adt, field_tys))
  }

  fn check_generic_count(
    span: Span,
    def: &Def<'core>,
    path: &GenericPath<'core>,
    expected: usize,
  ) -> Result<(), Diag<'core>> {
    if let Some(generics) = &path.generics {
      if generics.len() != expected {
        Err(Diag::BadGenericCount {
          span,
          path: def.canonical.clone(),
          expected,
          got: path.generics.as_ref().unwrap().len(),
        })?
      }
    }
    Ok(())
  }
}
