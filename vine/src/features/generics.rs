use std::mem::take;

use vine_util::{idx::Counter, parser::Parse};

use crate::{
  components::{charter::Charter, lexer::Token, parser::Parser, resolver::Resolver},
  structures::{
    ast::{
      GenericArgs, GenericParams, Generics, ImplKind, ImplParam, Path, Span, TraitKind, TyKind,
      TypeArg, TypeParam,
    },
    chart::{DefId, GenericsDef, GenericsId, GenericsKind},
    diag::Diag,
    signatures::ImplParams,
    tir::TirImpl,
    types::{ImplType, Type, TypeKind},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_generic_params(&mut self) -> Result<GenericParams, Diag> {
    self.parse_generics(true, Self::parse_type_param, Self::parse_impl_param)
  }

  pub(crate) fn parse_type_param(&mut self) -> Result<TypeParam, Diag> {
    let span = self.start_span();
    let name = self.parse_ident()?;
    let flex = self.parse_flex()?;
    let span = self.end_span(span);
    Ok(TypeParam { span, name, flex })
  }

  fn parse_impl_param(&mut self) -> Result<ImplParam, Diag> {
    let span = self.start_span();
    let trait_ = self.parse_trait()?;
    if let TraitKind::Path(path) = &*trait_.kind
      && let Some(name) = path.as_ident()
      && self.eat(Token::Colon)?
    {
      let trait_ = self.parse_trait()?;
      let span = self.end_span(span);
      Ok(ImplParam { span, name: Some(name), trait_ })
    } else {
      let span = self.end_span(span);
      Ok(ImplParam { span, name: None, trait_ })
    }
  }

  pub(crate) fn parse_generic_args(&mut self) -> Result<GenericArgs, Diag> {
    self.parse_generics(false, Self::parse_type_arg, Self::parse_impl)
  }

  fn parse_type_arg(&mut self) -> Result<TypeArg, Diag> {
    let span = self.start_span();
    let ty = self.parse_ty()?;
    if let TyKind::Path(path) = &*ty.kind
      && let Some(name) = path.as_ident()
      && self.eat(Token::Eq)?
    {
      let ty = self.parse_ty()?;
      let span = self.end_span(span);
      Ok(TypeArg { span, name: Some(name), ty })
    } else {
      let span = self.end_span(span);
      Ok(TypeArg { span, name: None, ty })
    }
  }

  fn parse_generics<T, I>(
    &mut self,
    can_inherit: bool,
    parse_t: impl FnMut(&mut Self) -> Result<T, Diag>,
    parse_i: impl FnMut(&mut Self) -> Result<I, Diag>,
  ) -> Result<Generics<T, I>, Diag> {
    let span = self.start_span();
    let mut inherit = false;
    let mut types = Vec::new();
    let mut impls = Vec::new();
    if self.eat(Token::OpenBracket)? {
      inherit = can_inherit && self.eat(Token::DotDotDot)?;
      self.parse_generics_section(&mut types, parse_t)?;
      self.parse_generics_section(&mut impls, parse_i)?;
      self.expect(Token::CloseBracket)?;
    }
    let span = self.end_span(span);
    Ok(Generics { span, inherit, types, impls })
  }

  fn parse_generics_section<E>(
    &mut self,
    section: &mut Vec<E>,
    mut parse_element: impl FnMut(&mut Self) -> Result<E, Diag>,
  ) -> Result<(), Diag> {
    while !self.check(Token::Semi) && !self.check(Token::CloseBracket) {
      section.push(parse_element(self)?);
      if !self.eat(Token::Comma)? {
        break;
      }
    }
    if !self.check(Token::CloseBracket) {
      self.expect(Token::Semi)?;
    }
    Ok(())
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_generic_params(&self, generics: &GenericParams) -> Doc<'src> {
    self.fmt_generics(
      generics.inherit,
      &generics.types,
      &generics.impls,
      |p| self.fmt_type_param(p),
      |p| self.fmt_impl_param(p),
    )
  }

  pub(crate) fn fmt_generic_args(&self, generics: &GenericArgs) -> Doc<'src> {
    let mut types = &*generics.types;
    let mut impls = &*generics.impls;
    while types.last().is_some_and(|t| t.name.is_none() && matches!(*t.ty.kind, TyKind::Hole)) {
      types = &types[..types.len() - 1];
    }
    while impls.last().is_some_and(|i| matches!(*i.kind, ImplKind::Hole)) {
      impls = &impls[..impls.len() - 1];
    }
    self.fmt_generics(false, types, impls, |t| self.fmt_type_arg(t), |p| self.fmt_impl(p))
  }

  pub(crate) fn fmt_type_param(&self, param: &TypeParam) -> Doc<'src> {
    Doc::concat([Doc(param.name.clone()), self.fmt_flex(param.flex)])
  }

  pub(crate) fn fmt_type_arg(&self, arg: &TypeArg) -> Doc<'src> {
    Doc::concat([
      if let Some(name) = &arg.name {
        Doc::concat([Doc(name.clone()), Doc(" = ")])
      } else {
        Doc::EMPTY
      },
      self.fmt_ty(&arg.ty),
    ])
  }

  fn fmt_impl_param(&self, param: &ImplParam) -> Doc<'src> {
    match param.name.clone() {
      Some(name) => Doc::concat([Doc(name), Doc(": "), self.fmt_trait(&param.trait_)]),
      None => self.fmt_trait(&param.trait_),
    }
  }

  pub(crate) fn fmt_generics<T, I>(
    &self,
    inherit: bool,
    types: &[T],
    impls: &[I],
    fmt_t: impl Fn(&T) -> Doc<'src>,
    fmt_i: impl Fn(&I) -> Doc<'src>,
  ) -> Doc<'src> {
    let include_types = !types.is_empty() || !impls.is_empty();
    let include_impls = !impls.is_empty();
    let sections = [
      include_types.then(|| self.fmt_generics_section(types, fmt_t)),
      include_impls.then(|| self.fmt_generics_section(impls, fmt_i)),
    ];
    let has_sections = sections.iter().any(|x| x.is_some());
    if !inherit && !has_sections {
      Doc::EMPTY
    } else {
      Doc::concat([
        Doc("["),
        Doc::group([
          if inherit {
            Doc::concat([Doc("..."), if has_sections { Doc::soft_line(" ") } else { Doc::EMPTY }])
          } else {
            Doc::EMPTY
          },
          Doc::interleave(
            sections.into_iter().flatten(),
            Doc::concat([Doc(";"), Doc::soft_line(" ")]),
          ),
          Doc::if_multi(";"),
        ]),
        Doc("]"),
      ])
    }
  }

  fn fmt_generics_section<E>(
    &self,
    section: &[E],
    fmt_element: impl Fn(&E) -> Doc<'src>,
  ) -> Doc<'src> {
    if section.is_empty() {
      Doc::EMPTY
    } else if let [element] = section {
      fmt_element(element)
    } else {
      Doc::bare_group([Doc::interleave(
        section.iter().map(fmt_element),
        Doc::concat([Doc(","), Doc::soft_line(" ")]),
      )])
    }
  }
}

impl Charter<'_> {
  pub(crate) fn chart_generics(
    &mut self,
    def: DefId,
    parent: GenericsId,
    generics: GenericParams,
    impl_allowed: bool,
  ) -> GenericsId {
    self.chart.generics.push(GenericsDef {
      span: generics.span,
      def,
      parent: generics.inherit.then_some(parent),
      kind: GenericsKind::Explicit { types: generics.types, impls: generics.impls },
      impl_allowed,
    })
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_type_params(&mut self, generics_id: GenericsId) {
    let generics_def = &self.chart.generics[generics_id];
    let mut type_params =
      generics_def.parent.map(|id| self.sigs.type_params[id].clone()).unwrap_or_default();
    let base_index = type_params.params.len();
    if let GenericsKind::Explicit { types, .. } = &generics_def.kind {
      for param in types {
        if let Some(&index) = type_params.lookup.get(&param.name)
          && index < base_index
        {
          continue;
        }
        let index = type_params.params.len();
        type_params.params.push(param.name.clone());
        if type_params.lookup.insert(param.name.clone(), index).is_some() {
          self.diags.error(Diag::DuplicateTypeParam { span: param.span });
        }
      }
    }
    self.sigs.type_params.push_to(generics_id, type_params);
  }

  pub(crate) fn resolve_impl_params(&mut self, generics_id: GenericsId) {
    let generics_def = &self.chart.generics[generics_id];
    self.initialize(generics_def.def, generics_id, false);
    let mut impl_params =
      generics_def.parent.map(|id| self.sigs.impl_params[id].clone()).unwrap_or_default();
    self.types = take(&mut impl_params.types.types);

    match &generics_def.kind {
      GenericsKind::None => {}

      GenericsKind::Explicit { types, impls } => {
        for param in types {
          let index = self.sigs.type_params[generics_id].lookup[&param.name];
          let span = param.span;
          let ty = self.types.new(TypeKind::Param(index, param.name.clone()));
          if param.flex.fork() {
            impl_params.types.inner.push(if let Some(fork) = self.chart.builtins.fork {
              ImplType::Trait(fork, vec![ty])
            } else {
              ImplType::Error(self.diags.error(Diag::MissingBuiltin { span, builtin: "Fork" }))
            });
          }
          if param.flex.drop() {
            impl_params.types.inner.push(if let Some(drop) = self.chart.builtins.drop {
              ImplType::Trait(drop, vec![ty])
            } else {
              ImplType::Error(self.diags.error(Diag::MissingBuiltin { span, builtin: "Drop" }))
            });
          }
        }

        for param in impls {
          let index = impl_params.types.inner.len();
          impl_params.types.inner.push(self.resolve_trait(&param.trait_));
          if let Some(name) = param.name.clone()
            && impl_params.lookup.insert(name, index).is_some()
          {
            self.diags.error(Diag::DuplicateImplParam { span: param.span });
          }
        }
      }

      GenericsKind::Trait { trait_id } => {
        let parent = generics_def.parent.unwrap();
        let type_params = self.sigs.type_params[parent]
          .params
          .iter()
          .enumerate()
          .map(|(index, name)| self.types.new(TypeKind::Param(index, name.clone())))
          .collect();
        impl_params.types.inner.push(ImplType::Trait(*trait_id, type_params));
      }

      GenericsKind::Derive(kind) => {
        if let Ok(trait_id) = self.resolve_derive_kind(kind) {
          for (index, name) in self.sigs.type_params[generics_id].params.iter().enumerate() {
            let ty = self.types.new(TypeKind::Param(index, name.clone()));
            impl_params.types.inner.push(ImplType::Trait(trait_id, vec![ty]))
          }
        }
      }
    }

    if !generics_def.impl_allowed && !impl_params.types.inner.is_empty() {
      impl_params = ImplParams::default();
      self.diags.error(Diag::UnexpectedImplParam { span: generics_def.span });
    }

    impl_params.types.types = take(&mut self.types);
    self.sigs.impl_params.push_to(generics_id, impl_params);
  }

  pub fn resolve_generics(
    &mut self,
    path: &Path,
    params_id: GenericsId,
    inference: bool,
  ) -> (Vec<Type>, Vec<TirImpl>) {
    self._resolve_generics(path.span, path.generics.as_ref(), params_id, inference, None)
  }

  pub(crate) fn _resolve_generics(
    &mut self,
    span: Span,
    args: Option<&GenericArgs>,
    generics_id: GenericsId,
    inference: bool,
    inferred_type_args: Option<Vec<Type>>,
  ) -> (Vec<Type>, Vec<TirImpl>) {
    let _args = GenericArgs::empty(span);
    let args = args.unwrap_or(&_args);
    let params = &self.chart.generics[generics_id];

    let type_args = self.resolve_type_args(span, &args.types, generics_id, inference);
    if let Some(inferred) = inferred_type_args {
      for (inferred, &passed) in inferred.into_iter().zip(type_args.iter()) {
        _ = self.types.unify(inferred, passed);
      }
    }

    let impl_param_count =
      // Things with no inference cannot have implementation parameters; skip
      // checking the signature as this may not have been resolved yet.
      if !inference { 0 } else { self.sigs.impl_params[generics_id].types.inner.len() };
    if args.impls.len() > impl_param_count {
      self.diags.error(Diag::BadGenericCount {
        span,
        path: self.chart.defs[params.def].path.clone(),
        expected: impl_param_count,
        got: args.impls.len(),
        kind: "impl",
      });
    }
    let impl_args = if impl_param_count != 0 {
      let impl_params_types =
        self.types.import(&self.sigs.impl_params[generics_id].types, Some(&type_args));
      impl_params_types
        .into_iter()
        .enumerate()
        .map(|(i, ty)| match args.impls.get(i) {
          Some(impl_) => self.resolve_impl_type(impl_, &ty),
          None => self.find_impl(args.span, &ty, false),
        })
        .collect()
    } else {
      Vec::new()
    };

    (type_args, impl_args)
  }

  fn resolve_type_args(
    &mut self,
    span: Span,
    args: &[TypeArg],
    generics_id: GenericsId,
    inference: bool,
  ) -> Vec<Type> {
    let path = || self.chart.defs[self.chart.generics[generics_id].def].path.clone();
    let type_param_count = self.sigs.type_params[generics_id].params.len();
    let mut type_args = vec![None; type_param_count];
    let mut positional = Counter(0);
    for arg in args {
      let idx = if let Some(name) = &arg.name {
        if let Some(&idx) = self.sigs.type_params[generics_id].lookup.get(name) {
          idx
        } else {
          self.diags.error(Diag::NoSuchTypeParam {
            span: arg.span,
            path: path(),
            name: name.clone(),
          });
          continue;
        }
      } else {
        positional.next()
      };
      let Some(ty) = type_args.get_mut(idx) else { continue };
      if ty.is_some() {
        self.diags.error(Diag::DuplicateTypeArg {
          span: arg.span,
          name: self.sigs.type_params[generics_id].params[idx].clone(),
        });
      } else {
        *ty = Some(self.resolve_ty(&arg.ty, inference));
      }
    }
    if positional.count() > type_param_count {
      self.diags.error(Diag::BadGenericCount {
        span,
        path: path(),
        expected: type_param_count,
        got: positional.count(),
        kind: "type",
      });
    }
    type_args
      .into_iter()
      .enumerate()
      .map(|(idx, ty)| {
        ty.unwrap_or_else(|| {
          if inference {
            self.types.new_var(span)
          } else {
            self.types.error(self.diags.error(Diag::MissingTypeArg {
              span,
              name: self.sigs.type_params[generics_id].params[idx].clone(),
            }))
          }
        })
      })
      .collect::<Vec<_>>()
  }

  pub(crate) fn show_generics(&self, generics: GenericsId, impl_params: bool) -> String {
    let type_params = &self.sigs.type_params[generics];
    let _impl_params = Default::default();
    let impl_params = if impl_params { &self.sigs.impl_params[generics] } else { &_impl_params };
    if type_params.params.is_empty() && impl_params.types.inner.is_empty() {
      return "".into();
    }
    let mut str = String::new();
    str += "[";
    let mut first = true;
    for param in &type_params.params {
      if !first {
        str += ", ";
      }
      first = false;
      str += &param.0;
    }
    if !impl_params.types.inner.is_empty() {
      str += "; ";
      first = true;
      for ty in &impl_params.types.inner {
        if !first {
          str += ", ";
        }
        first = false;
        str += &impl_params.types.types.show_impl_type(self.chart, ty);
      }
    }
    str += "]";
    str
  }
}
