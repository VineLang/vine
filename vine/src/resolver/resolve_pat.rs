use crate::{
  ast::{Pat, PatKind},
  chart::DefPatternKind,
  diag::Diag,
  resolver::{Binding, Form, Resolver, Type},
  tir::{TirPat, TirPatKind},
  types::TypeKind,
};

impl<'core> Resolver<'core, '_> {
  pub(super) fn resolve_pat_type(
    &mut self,
    pat: &Pat<'core>,
    form: Form,
    refutable: bool,
    ty: Type,
  ) -> TirPat {
    let pat = self.resolve_pat(pat, form, refutable);
    self.expect_type(pat.span, pat.ty, ty);
    pat
  }

  pub(super) fn resolve_pat(&mut self, pat: &Pat<'core>, form: Form, refutable: bool) -> TirPat {
    match self._resolve_pat(pat, form, refutable) {
      Ok((ty, kind)) => TirPat { span: pat.span, ty, form, kind: Box::new(kind) },
      Err(diag) => self.error_pat(pat.span, diag),
    }
  }

  pub(super) fn _resolve_pat(
    &mut self,
    pat: &Pat<'core>,
    form: Form,
    refutable: bool,
  ) -> Result<(Type, TirPatKind), Diag<'core>> {
    let span = pat.span;
    Ok(match (&pat.kind, form) {
      (_, Form::Error(_)) => unreachable!(),
      (PatKind::Struct(..) | PatKind::Enum(..) | PatKind::Local(_), _) => unreachable!(),
      (PatKind::Error(e), _) => Err(*e)?,

      (PatKind::Paren(p), _) => self._resolve_pat(p, form, refutable)?,

      (PatKind::Annotation(pat, ty), _) => {
        let ty = self.resolve_type(ty, true);
        (ty, *self.resolve_pat_type(pat, form, refutable, ty).kind)
      }

      (PatKind::Path(path, data), _) => {
        let resolved = self.resolve_path_to(self.cur_def, path, "pattern", |d| d.pattern_kind);
        match resolved {
          Ok(DefPatternKind::Struct(struct_id)) => {
            let data = match data {
              Some(args) => {
                if let [data] = &**args {
                  self.resolve_pat(data, form, refutable)
                } else {
                  let (ty, kind) = self._resolve_pat_tuple(form, refutable, args);
                  TirPat { span, ty, form, kind: Box::new(kind) }
                }
              }
              None => self.error_pat(span, Diag::ExpectedDataSubpattern { span }),
            };
            let (type_params, _) =
              self.resolve_generics(path, self.chart.structs[struct_id].generics, true);
            let data_ty = self.types.import(&self.sigs.structs[struct_id], Some(&type_params)).data;
            self.expect_type(data.span, data.ty, data_ty);
            let ty = self.types.new(TypeKind::Struct(struct_id, type_params));
            (ty, TirPatKind::Struct(struct_id, data))
          }
          Ok(DefPatternKind::Enum(enum_id, variant)) => {
            let data = data.as_ref().map(|args| {
              if let [data] = &**args {
                self.resolve_pat(data, form, refutable)
              } else {
                let (ty, kind) = self._resolve_pat_tuple(form, refutable, args);
                TirPat { span, ty, form, kind: Box::new(kind) }
              }
            });
            let (type_params, _) =
              self.resolve_generics(path, self.chart.enums[enum_id].generics, true);
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
                  self.core.report(Diag::EnumVariantNoData { span });
                }
                None
              }
            };
            if !refutable {
              self.core.report(Diag::ExpectedCompletePat { span });
            }
            let ty = self.types.new(TypeKind::Enum(enum_id, type_params));
            (ty, TirPatKind::Enum(enum_id, variant, data))
          }
          Err(diag) => {
            if let (Some(ident), None) = (path.as_ident(), data) {
              let local = self.locals.next();
              let ty = self.types.new_var(span);
              self.bind(ident, Binding::Local(local, ty));
              (ty, TirPatKind::Local(local))
            } else {
              Err(diag)?
            }
          }
        }
      }

      (PatKind::Hole, _) => (self.types.new_var(span), TirPatKind::Hole),
      (PatKind::Inverse(inner), _) => {
        let inner = self.resolve_pat(inner, form.inverse(), refutable);
        (inner.ty.inverse(), TirPatKind::Inverse(inner))
      }
      (PatKind::Tuple(elements), _) => self._resolve_pat_tuple(form, refutable, elements),
      (PatKind::Object(entries), _) => {
        let object =
          self.build_object(entries, |self_, pat| self_.resolve_pat(pat, form, refutable))?;
        let ty = self.types.new(TypeKind::Object(object.iter().map(|(&i, x)| (i, x.ty)).collect()));
        (ty, TirPatKind::Composite(object.into_values().collect()))
      }
      (PatKind::Deref(inner), Form::Place) => {
        let ty = self.types.new_var(span);
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        let inner = self.resolve_pat_type(inner, Form::Value, refutable, ref_ty);
        (ty, TirPatKind::Deref(inner))
      }

      (PatKind::Ref(inner), Form::Value | Form::Place) => {
        let inner = self.resolve_pat(inner, Form::Place, refutable);
        (self.types.new(TypeKind::Ref(inner.ty)), TirPatKind::Ref(inner))
      }

      (PatKind::Ref(_), Form::Space) => Err(Diag::RefSpacePat { span })?,
      (PatKind::Deref(_), _) => Err(Diag::DerefNonPlacePat { span })?,
    })
  }

  fn _resolve_pat_tuple(
    &mut self,
    form: Form,
    refutable: bool,
    elements: &[Pat<'core>],
  ) -> (Type, TirPatKind) {
    let elements =
      Vec::from_iter(elements.iter().map(|element| self.resolve_pat(element, form, refutable)));
    let ty = self.types.new(TypeKind::Tuple(elements.iter().map(|x| x.ty).collect()));
    (ty, TirPatKind::Composite(elements))
  }
}
