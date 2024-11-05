use crate::{
  ast::{GenericPath, Pat, PatKind, Span, Ty},
  checker::{Checker, Form, Type},
  diag::{report, Diag},
};

impl Checker<'_> {
  pub(super) fn check_pat_annotation(
    &mut self,
    pat: &mut Pat,
    annotation: Option<&mut Ty>,
    form: Form,
    refutable: bool,
  ) -> Type {
    match annotation {
      Some(ty) => {
        let mut ty = self.hydrate_type(ty, true);
        self.check_pat_type(pat, form, refutable, &mut ty);
        ty
      }
      None => self.check_pat(pat, form, refutable),
    }
  }

  pub(super) fn check_pat_type(
    &mut self,
    pat: &mut Pat,
    form: Form,
    refutable: bool,
    ty: &mut Type,
  ) {
    let mut found = self.check_pat(pat, form, refutable);
    if !self.unify(&mut found, ty) {
      self.diags.add(Diag::ExpectedTypeFound {
        span: pat.span,
        expected: self.display_type(ty),
        found: self.display_type(&found),
      });
    }
  }

  pub(super) fn check_pat(&mut self, pat: &mut Pat, form: Form, refutable: bool) -> Type {
    let span = pat.span;
    match (&mut pat.kind, form) {
      (_, Form::Error(_)) => unreachable!(),
      (PatKind::Error(e), _) => Type::Error(*e),

      (PatKind::Paren(p), _) => self.check_pat(p, form, refutable),

      (PatKind::Adt(path, fields), _) => {
        report!(self.diags, pat.kind; self.check_adt_pat(span, path, fields, form, refutable))
      }

      (PatKind::Hole, _) => self.new_var(span),
      (PatKind::Local(l), _) => {
        let old = self.state.locals.insert(*l, self.state.vars.len());
        debug_assert!(old.is_none());
        self.new_var(span)
      }
      (PatKind::Inverse(p), _) => self.check_pat(p, form.inverse(), refutable).inverse(),
      (PatKind::Tuple(t), _) => {
        Type::Tuple(t.iter_mut().map(|p| self.check_pat(p, form, refutable)).collect())
      }
      (PatKind::Move(p), Form::Place) => self.check_pat(p, Form::Value, refutable),
      (PatKind::Deref(p), Form::Place) => {
        let ty = self.new_var(span);
        self.check_pat_type(p, Form::Value, refutable, &mut Type::Ref(Box::new(ty.clone())));
        ty
      }

      (PatKind::Ref(p), Form::Value | Form::Place) => {
        Type::Ref(Box::new(self.check_pat(p, Form::Place, refutable)))
      }

      (PatKind::Ref(pat), Form::Space) => {
        let err = self.diags.add(Diag::RefSpacePat { span });
        pat.kind = PatKind::Error(err);
        Type::Error(err)
      }
      (PatKind::Deref(pat), _) => {
        let err = self.diags.add(Diag::DerefNonPlacePat { span });
        pat.kind = PatKind::Error(err);
        Type::Error(err)
      }
      (PatKind::Move(_), _) => {
        let err = self.diags.add(Diag::MoveNonPlacePat { span });
        pat.kind = PatKind::Error(err);
        Type::Error(err)
      }
    }
  }

  fn check_adt_pat(
    &mut self,
    span: Span,
    path: &mut GenericPath,
    fields: &mut Option<Vec<Pat>>,
    form: Form,
    refutable: bool,
  ) -> Result<Type, Diag> {
    let (adt, field_tys) = self.typeof_variant_def(path, refutable)?;
    let fields = fields.get_or_insert(Vec::new());
    if fields.len() != field_tys.len() {
      Err(Diag::BadFieldCount {
        span,
        path: self.defs[path.path.resolved.unwrap()].canonical.clone(),
        expected: field_tys.len(),
        got: fields.len(),
      })?
    }
    for (field, mut ty) in fields.iter_mut().zip(field_tys) {
      self.check_pat_type(field, form, refutable, &mut ty);
    }
    Ok(adt)
  }
}
