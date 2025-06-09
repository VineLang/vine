use crate::{
  ast::{Pat, PatKind},
  chart::DefPatternKind,
  diag::Diag,
  resolver::{Binding, Form, Resolver, Type},
  types::TypeKind,
};

impl<'core> Resolver<'core, '_> {
  pub(super) fn resolve_pat_type(
    &mut self,
    pat: &mut Pat<'core>,
    form: Form,
    refutable: bool,
    ty: Type,
  ) {
    let found = self.resolve_pat(pat, form, refutable);
    if self.types.unify(found, ty).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
        span: pat.span,
        expected: self.types.show(self.chart, ty),
        found: self.types.show(self.chart, found),
      });
    }
  }

  pub(super) fn resolve_pat(&mut self, pat: &mut Pat<'core>, form: Form, refutable: bool) -> Type {
    let span = pat.span;
    match (&mut pat.kind, form) {
      (_, Form::Error(_)) => unreachable!(),
      (PatKind::Struct(..) | PatKind::Enum(..) | PatKind::Local(_), _) => unreachable!(),
      (PatKind::Error(e), _) => self.types.error(*e),

      (PatKind::Paren(p), _) => self.resolve_pat(p, form, refutable),

      (PatKind::Annotation(pat, ty), _) => {
        let ty = self.resolve_type(ty, true);
        self.resolve_pat_type(pat, form, refutable, ty);
        ty
      }

      (PatKind::Path(path, data), _) => {
        let resolved = self.resolve_path_to(self.cur_def, path, "pattern", |d| d.pattern_kind);
        match resolved {
          Ok(DefPatternKind::Struct(struct_id)) => {
            let mut data = match data.take() {
              Some(mut args) => {
                if args.len() == 1 {
                  args.pop().unwrap()
                } else {
                  Pat { span, kind: PatKind::Tuple(args) }
                }
              }
              None => Pat {
                span,
                kind: PatKind::Error(self.core.report(Diag::ExpectedDataSubpattern { span })),
              },
            };
            let mut generics = path.take_generics();
            let type_params =
              self.resolve_generics(&mut generics, self.chart.structs[struct_id].generics, true);
            let data_ty = self.types.import(&self.sigs.structs[struct_id], Some(&type_params)).data;
            self.resolve_pat_type(&mut data, form, refutable, data_ty);
            pat.kind = PatKind::Struct(struct_id, generics, Box::new(data));
            self.types.new(TypeKind::Struct(struct_id, type_params))
          }
          Ok(DefPatternKind::Enum(enum_id, variant)) => {
            let mut data = data.take().map(|mut args| {
              if args.len() == 1 {
                args.pop().unwrap()
              } else {
                Pat { span, kind: PatKind::Tuple(args) }
              }
            });
            let mut generics = path.take_generics();
            let type_params =
              self.resolve_generics(&mut generics, self.chart.enums[enum_id].generics, true);
            let data_ty =
              self.types.import_with(&self.sigs.enums[enum_id], Some(&type_params), |t, sig| {
                Some(t.transfer(&sig.variant_data[variant]?))
              });
            match (&mut data, data_ty) {
              (None, None) => {}
              (Some(data), Some(data_ty)) => {
                self.resolve_pat_type(data, Form::Value, refutable, data_ty);
              }
              (None, Some(_)) => {
                self.core.report(Diag::ExpectedDataSubpattern { span });
              }
              (Some(_), None) => {
                self.core.report(Diag::EnumVariantNoData { span });
              }
            }
            pat.kind = PatKind::Enum(enum_id, variant, generics, data.map(Box::new));
            if !refutable {
              self.core.report(Diag::ExpectedCompletePat { span });
            }
            self.types.new(TypeKind::Enum(enum_id, type_params))
          }
          Err(diag) => {
            if let (Some(ident), None) = (path.as_ident(), data) {
              let local = self.locals.next();
              let ty = self.types.new_var(span);
              self.bind(ident, Binding::Local(local, ty));
              pat.kind = PatKind::Local(local);
              ty
            } else {
              let err = self.core.report(diag);
              pat.kind = PatKind::Error(err);
              self.types.error(err)
            }
          }
        }
      }

      (PatKind::Hole, _) => self.types.new_var(span),
      (PatKind::Inverse(inner), _) => {
        let inner = self.resolve_pat(inner, form.inverse(), refutable);
        inner.inverse()
      }
      (PatKind::Tuple(els), _) => {
        let els = els.iter_mut().map(|p| self.resolve_pat(p, form, refutable)).collect();
        self.types.new(TypeKind::Tuple(els))
      }
      (PatKind::Object(e), _) => {
        self.build_object_type(e, |self_, p| self_.resolve_pat(p, form, refutable))
      }
      (PatKind::Deref(p), Form::Place) => {
        let ty = self.types.new_var(span);
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        self.resolve_pat_type(p, Form::Value, refutable, ref_ty);
        ty
      }

      (PatKind::Ref(inner), Form::Value | Form::Place) => {
        let inner = self.resolve_pat(inner, Form::Place, refutable);
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
