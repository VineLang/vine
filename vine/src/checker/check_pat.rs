use crate::{
  ast::{GenericArgs, Pat, PatKind, Span},
  chart::{AdtId, VariantId},
  checker::{Checker, Form, Type},
  diag::{report, Diag},
};

impl<'core> Checker<'core, '_> {
  pub(super) fn check_pat_type(
    &mut self,
    pat: &mut Pat<'core>,
    form: Form,
    refutable: bool,
    ty: &mut Type<'core>,
  ) {
    let mut found = self.check_pat(pat, form, refutable);
    if !self.unifier.unify(&mut found, ty) {
      self.core.report(Diag::ExpectedTypeFound {
        span: pat.span,
        expected: self.display_type(ty),
        found: self.display_type(&found),
      });
    }
  }

  pub(super) fn check_pat(
    &mut self,
    pat: &mut Pat<'core>,
    form: Form,
    refutable: bool,
  ) -> Type<'core> {
    let span = pat.span;
    match (&mut pat.kind, form) {
      (_, Form::Error(_)) => unreachable!(),
      (PatKind::PathCall(..), _) => unreachable!(),
      (PatKind::Error(e), _) => Type::Error(*e),

      (PatKind::Paren(p), _) => self.check_pat(p, form, refutable),

      (PatKind::Annotation(pat, ty), _) => {
        let mut ty = self.hydrate_type(ty, true);
        self.check_pat_type(pat, form, refutable, &mut ty);
        ty
      }

      (PatKind::Adt(adt, variant, generics, fields), _) => {
        report!(self.core, pat.kind; self.check_adt_pat(span, *adt, *variant, generics, fields, form, refutable))
      }

      (PatKind::Hole, _) => self.unifier.new_var(span),
      (PatKind::Local(l), _) => {
        let var = self.unifier._new_var(span);
        let old = self.locals.insert(*l, var);
        debug_assert!(old.is_none());
        Type::Var(var)
      }
      (PatKind::Inverse(p), _) => self.check_pat(p, form.inverse(), refutable).inverse(),
      (PatKind::Tuple(t), _) => {
        Type::Tuple(t.iter_mut().map(|p| self.check_pat(p, form, refutable)).collect())
      }
      (PatKind::Object(e), _) => {
        report!(self.core, pat.kind; self.build_object_type(e, |self_, p| self_.check_pat(p, form, refutable)))
      }
      (PatKind::Deref(p), Form::Place) => {
        let ty = self.unifier.new_var(span);
        self.check_pat_type(p, Form::Value, refutable, &mut Type::Ref(Box::new(ty.clone())));
        ty
      }

      (PatKind::Ref(p), Form::Value | Form::Place) => {
        Type::Ref(Box::new(self.check_pat(p, Form::Place, refutable)))
      }

      (PatKind::Ref(pat), Form::Space) => {
        let err = self.core.report(Diag::RefSpacePat { span });
        pat.kind = PatKind::Error(err);
        Type::Error(err)
      }
      (PatKind::Deref(pat), _) => {
        let err = self.core.report(Diag::DerefNonPlacePat { span });
        pat.kind = PatKind::Error(err);
        Type::Error(err)
      }
    }
  }

  #[allow(clippy::too_many_arguments)]
  fn check_adt_pat(
    &mut self,
    span: Span,
    adt: AdtId,
    variant: VariantId,
    generics: &mut GenericArgs<'core>,
    fields: &mut [Pat<'core>],
    form: Form,
    refutable: bool,
  ) -> Result<Type<'core>, Diag<'core>> {
    let type_params = self.check_generics(generics, self.chart.adts[adt].generics, true);
    let field_tys = self.types.adt_types[adt][variant]
      .iter()
      .map(|t| t.instantiate(&type_params))
      .collect::<Vec<_>>();
    if fields.len() != field_tys.len() {
      Err(Diag::BadFieldCount {
        span,
        path: self.chart.defs[self.chart.adts[adt].def].path,
        expected: field_tys.len(),
        got: fields.len(),
      })?
    }
    for (field, mut ty) in fields.iter_mut().zip(field_tys) {
      self.check_pat_type(field, form, refutable, &mut ty);
    }
    if !refutable && self.chart.adts[adt].variants.len() != 1 {
      return Err(Diag::ExpectedCompletePat { span });
    }
    Ok(Type::Adt(adt, type_params))
  }
}
