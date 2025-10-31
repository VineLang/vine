use vine_util::parser::Parser;

use crate::{
  components::{
    lexer::Token,
    parser::{VineParser, PATH},
    resolver::{Binding, Resolver},
  },
  structures::{
    ast::{Expr, ExprKind, Ident, Pat, PatKind, Path, Span},
    chart::{
      Def, DefId, DefImplKind, DefPatternKind, DefTypeKind, DefValueKind, MemberKind, WithVis,
    },
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirImpl, TirLocal, TirPat, TirPatKind},
    types::{ImplType, Type, TypeKind},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl VineParser<'_> {
  pub(crate) fn parse_path(&mut self) -> Result<Path, Diag> {
    let span = self.start_span();
    let absolute = self.eat(Token::ColonColon)?;
    let segments = self.parse_delimited(PATH, Self::parse_ident)?;
    let generics = self.check_then(Token::OpenBracket, Self::parse_generic_args)?;
    let span = self.end_span(span);
    Ok(Path { span, absolute, segments, generics })
  }

  pub(crate) fn parse_expr_path(&mut self) -> Result<ExprKind, Diag> {
    let path = self.parse_path()?;
    let args = self.check_then(Token::OpenParen, Self::parse_exprs)?;
    Ok(ExprKind::Path(path, args))
  }

  pub(crate) fn parse_pat_path(&mut self) -> Result<PatKind, Diag> {
    let path = self.parse_path()?;
    let data = self.check_then(Token::OpenParen, Self::parse_pats)?;
    Ok(PatKind::Path(path, data))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_path(&self, path: &Path) -> Doc<'src> {
    let mut docs = Vec::<Doc>::new();
    if path.absolute {
      docs.push(Doc("::"));
    }
    let mut first = true;
    for &seg in &path.segments {
      if !first {
        docs.push(Doc("::"));
      }
      docs.push(Doc(seg));
      first = false;
    }
    if let Some(generics) = &path.generics {
      docs.push(self.fmt_generic_args(generics));
    }
    Doc::concat_vec(docs)
  }

  pub(crate) fn fmt_expr_path(&self, path: &Path, args: &Option<Vec<Expr>>) -> Doc<'src> {
    match args {
      Some(args) => {
        Doc::concat([self.fmt_path(path), Doc::paren_comma(args.iter().map(|x| self.fmt_expr(x)))])
      }
      None => self.fmt_path(path),
    }
  }

  pub(crate) fn fmt_pat_path(&self, path: &Path, args: &Option<Vec<Pat>>) -> Doc<'src> {
    match args {
      Some(args) => {
        Doc::concat([self.fmt_path(path), Doc::paren_comma(args.iter().map(|x| self.fmt_pat(x)))])
      }
      None => self.fmt_path(path),
    }
  }
}

impl Resolver<'_> {
  pub fn resolve_path<T>(
    &mut self,
    base: DefId,
    path: &Path,
    desc: &'static str,
    f: impl FnOnce(&Def) -> Option<WithVis<T>>,
  ) -> Result<T, Diag> {
    let def = self._resolve_path(base, path)?;
    let def = &self.chart.defs[def];
    match f(def) {
      Some(WithVis { vis, kind }) => {
        if self.chart.visible(vis, base) {
          Ok(kind)
        } else {
          Err(Diag::InvisibleAssociated {
            span: path.span,
            desc,
            path: def.path,
            vis: self.chart.defs[vis].path,
          })
        }
      }
      None => Err(Diag::PathNoAssociated { span: path.span, desc, path: def.path }),
    }
  }

  fn _resolve_path(&mut self, source: DefId, path: &Path) -> Result<DefId, Diag> {
    let mut segments = path.segments.iter();
    let mut base = if path.absolute {
      DefId::ROOT
    } else {
      let initial = *segments.next().unwrap();
      self.resolve_initial(path.span, source, initial)?
    };
    for &segment in segments {
      base = self.resolve_segment(path.span, source, base, segment)?;
    }
    Ok(base)
  }

  pub(crate) fn resolve_initial(
    &mut self,
    span: Span,
    base: DefId,
    ident: Ident,
  ) -> Result<DefId, Diag> {
    let mut cur = base;
    loop {
      if let Some(resolved) = self._resolve_segment(span, base, cur, ident)? {
        return Ok(resolved);
      }
      if let Some(parent) = self.chart.defs[cur].parent {
        cur = parent;
      } else {
        break;
      }
    }
    if let Some(prelude) = self.chart.builtins.prelude {
      if let Some(resolved) = self._resolve_segment(span, base, prelude, ident)? {
        return Ok(resolved);
      }
    }
    Err(Diag::CannotResolve { span, module: self.chart.defs[base].path, ident })
  }

  pub(crate) fn resolve_segment(
    &mut self,
    span: Span,
    source: DefId,
    base: DefId,
    ident: Ident,
  ) -> Result<DefId, Diag> {
    let resolved = self._resolve_segment(span, source, base, ident)?;
    resolved.ok_or(Diag::CannotResolve { span, module: self.chart.defs[base].path, ident })
  }

  fn _resolve_segment(
    &mut self,
    span: Span,
    source: DefId,
    base: DefId,
    ident: Ident,
  ) -> Result<Option<DefId>, Diag> {
    let def = &self.chart.defs[base];

    if let Some(member) = def.members_lookup.get(&ident) {
      let vis = member.vis;
      let result = match member.kind {
        MemberKind::Child(result) => result,
        MemberKind::Import(import) => self.resolve_import(import)?,
      };
      if self.chart.visible(vis, source) {
        Ok(Some(result))
      } else {
        Err(Diag::Invisible {
          span,
          module: self.chart.defs[base].path,
          ident,
          vis: self.chart.defs[vis].path,
        })
      }
    } else {
      Ok(None)
    }
  }

  pub(crate) fn resolve_expr_path(
    &mut self,
    expr: &Expr,
    span: Span,
    path: &Path,
    args: &Option<Vec<Expr>>,
  ) -> Result<TirExpr, Diag> {
    if let Some(ident) = path.as_ident() {
      if let Some(bind) = self.scope.get(&ident).and_then(|x| x.last()) {
        let expr = match bind.binding {
          Binding::Local(local, _, ty) => TirExpr::new(span, ty, TirExprKind::Local(local)),
          Binding::Closure(id, ty) => TirExpr::new(span, ty, TirExprKind::Closure(id)),
        };
        return if let Some(args) = args {
          self._resolve_expr_call(span, expr, args)
        } else {
          Ok(expr)
        };
      }
    }
    match self.resolve_path(self.cur_def, path, "value", |d| d.value_kind) {
      Ok(DefValueKind::Const(const_id)) => {
        let expr = self.resolve_expr_path_const(span, path, const_id)?;
        if let Some(args) = args {
          self._resolve_expr_call(span, expr, args)
        } else {
          Ok(expr)
        }
      }
      Ok(DefValueKind::Fn(fn_id)) => self.resolve_expr_path_fn(span, path, fn_id, args),
      Ok(DefValueKind::Struct(struct_id)) => {
        self.resolve_expr_path_struct(expr, span, path, struct_id, args)
      }
      Ok(DefValueKind::Enum(enum_id, variant_id)) => {
        self.resolve_expr_path_enum(span, path, enum_id, variant_id, args)
      }
      Err(diag) => Err(diag)?,
    }
  }

  pub(crate) fn resolve_pat_path(
    &mut self,
    span: Span,
    path: &Path,
    data: &Option<Vec<Pat>>,
  ) -> Result<TirPat, Diag> {
    let resolved = self.resolve_path(self.cur_def, path, "pattern", |d| d.pattern_kind);
    match resolved {
      Ok(DefPatternKind::Struct(struct_id)) => {
        self.resolve_pat_path_struct(span, path, data, struct_id)
      }
      Ok(DefPatternKind::Enum(enum_id, variant)) => {
        self.resolve_pat_path_enum(span, path, data, enum_id, variant)
      }
      Err(diag) => {
        if let (Some(ident), None) = (path.as_ident(), data) {
          let ty = self.types.new_var(span);
          let local = self.locals.push(TirLocal { span, ty });
          self.bind(ident, Binding::Local(local, span, ty));
          Ok(TirPat::new(span, ty, TirPatKind::Local(local)))
        } else {
          Err(diag)?
        }
      }
    }
  }

  pub(crate) fn resolve_pat_sig_path(&mut self, span: Span, path: &Path, inference: bool) -> Type {
    let resolved = self.resolve_path(self.cur_def, path, "pattern", |d| d.pattern_kind);
    match resolved {
      Ok(DefPatternKind::Struct(struct_id)) => self.resolve_pat_sig_path_struct(path, struct_id),
      Ok(DefPatternKind::Enum(enum_id, _)) => self.resolve_pat_sig_path_enum(path, enum_id),
      Err(_) => {
        if inference {
          self.types.new_var(span)
        } else {
          self.types.error(self.core.report(Diag::ItemTypeHole { span }))
        }
      }
    }
  }

  pub(crate) fn resolve_ty_path(&mut self, path: &Path, inference: bool) -> Type {
    if let Some(ident) = path.as_ident() {
      if let Some(&index) = self.sigs.type_params[self.cur_generics].lookup.get(&ident) {
        return self.types.new(TypeKind::Param(index, ident));
      }
    }
    let resolved = self.resolve_path(self.cur_def, path, "type", |d| d.type_kind);
    match resolved {
      Ok(DefTypeKind::Opaque(opaque_id)) => self.resolve_ty_path_opaque(path, inference, opaque_id),
      Ok(DefTypeKind::Struct(struct_id)) => self.resolve_ty_path_struct(path, inference, struct_id),
      Ok(DefTypeKind::Enum(enum_id)) => self.resolve_ty_path_enum(path, inference, enum_id),
      Ok(DefTypeKind::Alias(type_alias_id)) => {
        self.resolve_ty_path_alias(path, inference, type_alias_id)
      }
      Err(diag) => self.types.error(self.core.report(diag)),
    }
  }

  pub(crate) fn resolve_impl_path(&mut self, path: &Path, ty: &ImplType) -> TirImpl {
    if let Some(ident) = path.as_ident() {
      let impl_params = &self.sigs.impl_params[self.cur_generics];
      if let Some(&index) = impl_params.lookup.get(&ident) {
        let actual_ty =
          self.types.import_with(&impl_params.types, None, |t, tys| t.transfer(&tys[index]));
        if self.types.unify_impl_type(&actual_ty, ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span: path.span,
            expected: self.types.show_impl_type(self.chart, ty),
            found: self.types.show_impl_type(self.chart, &actual_ty),
          });
        }
        return TirImpl::Param(index);
      }
    }
    match self.resolve_path(self.cur_def, path, "impl", |d| d.impl_kind) {
      Ok(DefImplKind::Impl(id)) => self.resolve_impl_path_impl(path, id, ty),
      Err(diag) => TirImpl::Error(self.core.report(diag)),
    }
  }
}
