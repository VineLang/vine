use crate::{
  ast::{Pat, PatKind},
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
      (PatKind::Path(..), _) => unreachable!(),
      (PatKind::Error(e), _) => Type::Error(*e),

      (PatKind::Paren(p), _) => self.check_pat(p, form, refutable),

      (PatKind::Annotation(pat, ty), _) => {
        let mut ty = self.hydrate_type(ty, true);
        self.check_pat_type(pat, form, refutable, &mut ty);
        ty
      }

      (PatKind::Struct(struct_id, generics, data), _) => {
        let type_params =
          self.check_generics(generics, self.chart.structs[*struct_id].generics, true);
        let mut data_ty = self.types.struct_types[*struct_id].instantiate(&type_params);
        self.check_pat_type(data, form, refutable, &mut data_ty);
        Type::Struct(*struct_id, type_params)
      }
      (PatKind::Enum(enum_id, variant, generics, data), _) => {
        let type_params = self.check_generics(generics, self.chart.enums[*enum_id].generics, true);
        let data_ty = &self.types.enum_types[*enum_id][*variant];
        match (data, data_ty) {
          (None, None) => {}
          (Some(data), Some(data_ty)) => {
            self.check_pat_type(
              data,
              Form::Value,
              refutable,
              &mut data_ty.instantiate(&type_params),
            );
          }
          (None, Some(_)) => {
            self.core.report(Diag::ExpectedDataSubpattern { span });
          }
          (Some(_), None) => {
            self.core.report(Diag::EnumVariantNoData { span });
          }
        }
        if !refutable {
          self.core.report(Diag::ExpectedCompletePat { span });
        }
        Type::Enum(*enum_id, type_params)
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
}
