use crate::{
  ast::{Pat, PatKind},
  checker::{Checker, Form, Type},
  diag::Diag,
  types::TypeKind,
};

impl<'core> Checker<'core, '_> {
  pub(super) fn check_pat_type(
    &mut self,
    pat: &mut Pat<'core>,
    form: Form,
    refutable: bool,
    ty: Type,
  ) {
    let found = self.check_pat(pat, form, refutable);
    if self.types.unify(found, ty).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
        span: pat.span,
        expected: self.types.show(self.chart, ty),
        found: self.types.show(self.chart, found),
      });
    }
  }

  pub(super) fn check_pat(&mut self, pat: &mut Pat<'core>, form: Form, refutable: bool) -> Type {
    let span = pat.span;
    match (&mut pat.kind, form) {
      (_, Form::Error(_)) => unreachable!(),
      (PatKind::Path(..), _) => unreachable!(),
      (PatKind::Error(e), _) => self.types.error(*e),

      (PatKind::Paren(p), _) => self.check_pat(p, form, refutable),

      (PatKind::Annotation(pat, ty), _) => {
        let ty = self.hydrate_type(ty, true);
        self.check_pat_type(pat, form, refutable, ty);
        ty
      }

      (PatKind::Struct(struct_id, generics, data), _) => {
        let type_params =
          self.check_generics(generics, self.chart.structs[*struct_id].generics, true);
        let data_ty =
          self.types.import(&self.sigs.structs[*struct_id], Some(&type_params), |t, sig| {
            t.transfer(sig.data)
          });
        self.check_pat_type(data, form, refutable, data_ty);
        self.types.new(TypeKind::Struct(*struct_id, type_params))
      }
      (PatKind::Enum(enum_id, variant, generics, data), _) => {
        let type_params = self.check_generics(generics, self.chart.enums[*enum_id].generics, true);
        let data_ty =
          self.types.import(&self.sigs.enums[*enum_id], Some(&type_params), |t, sig| {
            Some(t.transfer(sig.variant_data[*variant]?))
          });
        match (data, data_ty) {
          (None, None) => {}
          (Some(data), Some(data_ty)) => {
            self.check_pat_type(data, Form::Value, refutable, data_ty);
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
        self.types.new(TypeKind::Enum(*enum_id, type_params))
      }

      (PatKind::Hole, _) => self.types.new_var(span),
      (PatKind::Local(l), _) => {
        let ty = self.types.new_var(span);
        let old = self.locals.insert(*l, ty);
        debug_assert!(old.is_none());
        ty
      }
      (PatKind::Inverse(inner), _) => {
        let inner = self.check_pat(inner, form.inverse(), refutable);
        inner.inverse()
      }
      (PatKind::Tuple(els), _) => {
        let els = els.iter_mut().map(|p| self.check_pat(p, form, refutable)).collect();
        self.types.new(TypeKind::Tuple(els))
      }
      (PatKind::Object(e), _) => {
        self.build_object_type(e, |self_, p| self_.check_pat(p, form, refutable))
      }
      (PatKind::Deref(p), Form::Place) => {
        let ty = self.types.new_var(span);
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        self.check_pat_type(p, Form::Value, refutable, ref_ty);
        ty
      }

      (PatKind::Ref(inner), Form::Value | Form::Place) => {
        let inner = self.check_pat(inner, Form::Place, refutable);
        self.types.new(TypeKind::Ref(inner))
      }

      (PatKind::Ref(pat), Form::Space) => {
        let err = self.core.report(Diag::RefSpacePat { span });
        pat.kind = PatKind::Error(err);
        self.types.error(err)
      }
      (PatKind::Deref(pat), _) => {
        let err = self.core.report(Diag::DerefNonPlacePat { span });
        pat.kind = PatKind::Error(err);
        self.types.error(err)
      }
    }
  }
}
