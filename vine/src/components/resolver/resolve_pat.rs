use crate::{
  components::resolver::{Binding, Resolver, Type},
  structures::{
    ast::{Pat, PatKind},
    chart::DefPatternKind,
    diag::Diag,
    tir::{TirPat, TirPatKind},
    types::TypeKind,
  },
};

impl<'core> Resolver<'core, '_> {
  pub(super) fn resolve_pat_type(&mut self, pat: &Pat<'core>, ty: Type) -> TirPat {
    let span = pat.span;
    let pat = self.resolve_pat(pat);
    match self.expect_type(pat.span, pat.ty, ty) {
      Some(err) => self.error_pat(span, err.into()),
      None => pat,
    }
  }

  pub(super) fn resolve_pat(&mut self, pat: &Pat<'core>) -> TirPat {
    match self._resolve_pat(pat) {
      Ok((ty, kind)) => TirPat { span: pat.span, ty, kind: Box::new(kind) },
      Err(diag) => self.error_pat(pat.span, diag),
    }
  }

  pub(super) fn _resolve_pat(
    &mut self,
    pat: &Pat<'core>,
  ) -> Result<(Type, TirPatKind), Diag<'core>> {
    let span = pat.span;
    Ok(match &pat.kind {
      PatKind::Error(e) => Err(*e)?,

      PatKind::Paren(p) => self._resolve_pat(p)?,

      PatKind::Annotation(pat, ty) => {
        let ty = self.resolve_type(ty, true);
        (ty, *self.resolve_pat_type(pat, ty).kind)
      }

      PatKind::Path(path, data) => {
        let resolved = self.resolve_path_to(self.cur_def, path, "pattern", |d| d.pattern_kind);
        match resolved {
          Ok(DefPatternKind::Struct(struct_id)) => {
            let data = match data {
              Some(args) => {
                if let [data] = &**args {
                  self.resolve_pat(data)
                } else {
                  let (ty, kind) = self._resolve_pat_tuple(args);
                  TirPat { span, ty, kind: Box::new(kind) }
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
                self.resolve_pat(data)
              } else {
                let (ty, kind) = self._resolve_pat_tuple(args);
                TirPat { span, ty, kind: Box::new(kind) }
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
                  Err(Diag::EnumVariantNoData { span })?
                }
                None
              }
            };
            let ty = self.types.new(TypeKind::Enum(enum_id, type_params));
            (ty, TirPatKind::Enum(enum_id, variant, data))
          }
          Err(diag) => {
            if let (Some(ident), None) = (path.as_ident(), data) {
              let ty = self.types.new_var(span);
              let local = self.locals.push(ty);
              self.bind(ident, Binding::Local(local, ty));
              (ty, TirPatKind::Local(local))
            } else {
              Err(diag)?
            }
          }
        }
      }

      PatKind::Hole => (self.types.new_var(span), TirPatKind::Hole),
      PatKind::Inverse(inner) => {
        let inner = self.resolve_pat(inner);
        (inner.ty.inverse(), TirPatKind::Inverse(inner))
      }
      PatKind::Tuple(elements) => self._resolve_pat_tuple(elements),
      PatKind::Object(entries) => {
        let object = self.build_object(entries, |self_, pat| self_.resolve_pat(pat))?;
        let ty = self.types.new(TypeKind::Object(object.iter().map(|(&i, x)| (i, x.ty)).collect()));
        (ty, TirPatKind::Composite(object.into_values().collect()))
      }
      PatKind::Deref(inner) => {
        let ty = self.types.new_var(span);
        let ref_ty = self.types.new(TypeKind::Ref(ty));
        let inner = self.resolve_pat_type(inner, ref_ty);
        (ty, TirPatKind::Deref(inner))
      }

      PatKind::Ref(inner) => {
        let inner = self.resolve_pat(inner);
        (self.types.new(TypeKind::Ref(inner.ty)), TirPatKind::Ref(inner))
      }
    })
  }

  fn _resolve_pat_tuple(&mut self, elements: &[Pat<'core>]) -> (Type, TirPatKind) {
    let elements = Vec::from_iter(elements.iter().map(|element| self.resolve_pat(element)));
    let ty = self.types.new(TypeKind::Tuple(elements.iter().map(|x| x.ty).collect()));
    (ty, TirPatKind::Composite(elements))
  }
}
