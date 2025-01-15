use crate::{
  ast::{Path, Impl, ImplKind, Span},
  checker::{Checker, Type},
  diag::{Diag, ErrorGuaranteed},
  resolver::Def,
};

use super::report;

impl<'core> Checker<'core, '_> {
  pub(super) fn typeof_value_def(
    &mut self,
    path: &mut Path<'core>,
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
    let generic_count = value_def.type_params.len();
    Self::check_type_param_count(span, def, path, generic_count)?;
    let type_params = self.hydrate_generics(path, generic_count, true);
    let def = &self.resolver.defs[def_id];
    let value_def = def.value_def.as_ref().unwrap();
    let impl_params = path.generics.as_mut().map(|x| &mut x.impls[..]).unwrap_or(&mut []);
    let impl_param_tys = value_def
      .impl_param_tys
      .as_ref()
      .unwrap()
      .iter()
      .map(|t| t.instantiate(&type_params))
      .collect::<Vec<_>>();
    if impl_params.len() != impl_param_tys.len() {
      return Err(Diag::BadGenericCount {
        span,
        path: def.canonical.clone(),
        expected: impl_param_tys.len(),
        got: impl_params.len(),
        kind: "impl",
      });
    }
    for (impl_, mut expected) in impl_params.iter_mut().zip(impl_param_tys) {
      let mut ty = self.check_impl(impl_);
      if !self.unify(&mut ty, &mut expected) {
        self.core.report(Diag::ExpectedTypeFound {
          span: impl_.span,
          expected: self.display_type(&expected),
          found: self.display_type(&ty),
        });
      }
    }
    let def = &self.resolver.defs[def_id];
    let value_def = def.value_def.as_ref().unwrap();
    Ok(value_def.ty.as_ref().unwrap().instantiate(&type_params))
  }

  pub(super) fn typeof_impl_def(
    &mut self,
    path: &mut Path<'core>,
  ) -> Result<Type<'core>, Diag<'core>> {
    let span = path.span;
    let def_id = path.path.resolved.unwrap();
    let def = &self.resolver.defs[def_id];
    let Some(impl_def) = &def.impl_def else {
      return Err(Diag::PathNoImpl { span, path: def.canonical.clone() });
    };
    if !self.resolver.visible(impl_def.vis, self.cur_def) {
      return Err(Diag::ImplInvisible {
        span,
        path: def.canonical.clone(),
        vis: self.resolver.defs[impl_def.vis].canonical.clone(),
      });
    }
    let generic_count = impl_def.type_params.len();
    Self::check_type_param_count(span, def, path, generic_count)?;
    let type_params = self.hydrate_generics(path, generic_count, true);
    let def = &self.resolver.defs[def_id];
    let impl_def = def.impl_def.as_ref().unwrap();
    let impl_params = path.generics.as_mut().map(|x| &mut x.impls[..]).unwrap_or(&mut []);
    let impl_param_tys = impl_def
      .impl_param_tys
      .as_ref()
      .unwrap()
      .iter()
      .map(|t| t.instantiate(&type_params))
      .collect::<Vec<_>>();
    if impl_params.len() != impl_param_tys.len() {
      return Err(Diag::BadGenericCount {
        span,
        path: def.canonical.clone(),
        expected: impl_param_tys.len(),
        got: impl_params.len(),
        kind: "impl",
      });
    }
    for (impl_, mut expected) in impl_params.iter_mut().zip(impl_param_tys) {
      let mut ty = self.check_impl(impl_);
      if !self.unify(&mut ty, &mut expected) {
        self.core.report(Diag::ExpectedTypeFound {
          span: impl_.span,
          expected: self.display_type(&expected),
          found: self.display_type(&ty),
        });
      }
    }
    let def = &self.resolver.defs[def_id];
    let impl_def = def.impl_def.as_ref().unwrap();
    let ty = impl_def
      .trait_ty
      .clone()
      .map(|(a, b)| Type::Trait(a, b))
      .unwrap_or(Type::Error(ErrorGuaranteed::new_unchecked()))
      .instantiate(&type_params);
    Ok(ty)
  }

  fn check_impl(&mut self, impl_: &mut Impl<'core>) -> Type<'core> {
    let span = impl_.span;
    match &mut impl_.kind {
      ImplKind::Hole => Type::Error(self.core.report(Diag::UnspecifiedImpl { span })),
      ImplKind::Param(n) => self.impl_params[*n].clone(),
      ImplKind::Path(path) => report!(self.core; self.typeof_impl_def(path)),
      ImplKind::Error(e) => Type::Error(*e),
    }
  }

  pub(super) fn typeof_type_def(
    &mut self,
    path: &mut Path<'core>,
    inference: bool,
  ) -> Result<Type<'core>, Diag<'core>> {
    let span = path.span;
    if path.generics.as_ref().is_some_and(|g| !g.impls.is_empty()) {
      self.core.report(Diag::UnexpectedImplArgs { span });
    }
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
    let generic_count = type_def.type_params.len();
    Self::check_type_param_count(span, def, path, generic_count)?;
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
    path: &mut Path<'core>,
    refutable: bool,
    inference: bool,
  ) -> Result<(Type<'core>, Option<Vec<Type<'core>>>), Diag<'core>> {
    let span = path.span;
    if path.generics.as_ref().is_some_and(|g| !g.impls.is_empty()) {
      self.core.report(Diag::UnexpectedImplArgs { span });
    }
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
    let generic_count = adt_def.type_params.len();
    Self::check_type_param_count(span, variant, path, generic_count)?;
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

  fn check_type_param_count(
    span: Span,
    def: &Def<'core>,
    path: &Path<'core>,
    expected: usize,
  ) -> Result<(), Diag<'core>> {
    if let Some(generics) = &path.generics {
      if !generics.types.is_empty() && generics.types.len() != expected {
        Err(Diag::BadGenericCount {
          span,
          path: def.canonical.clone(),
          expected,
          got: generics.types.len(),
          kind: "type",
        })?
      }
    }
    Ok(())
  }
}
