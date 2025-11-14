use std::mem::take;

use vine_util::{idx::RangeExt, parser::Parser};

use crate::{
  components::{charter::Charter, lexer::Token, parser::VineParser, resolver::Resolver},
  structures::{
    ast::{
      Flex, GenericArgs, GenericParams, Generics, ImplParam, Path, Span, Trait, TraitKind,
      TypeParam,
    },
    chart::{DefId, GenericsDef, GenericsId},
    diag::{Diag, ErrorGuaranteed},
    signatures::ImplParams,
    tir::TirImpl,
    types::{ImplType, Type, TypeKind},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl VineParser<'_> {
  pub(crate) fn parse_generic_params(&mut self) -> Result<GenericParams, Diag> {
    self.parse_generics(true, Self::parse_type_param, Self::parse_impl_param)
  }

  fn parse_type_param(&mut self) -> Result<TypeParam, Diag> {
    let span = self.start_span();
    let name = self.parse_ident()?;
    let flex = self.parse_flex()?;
    let span = self.end_span(span);
    Ok(TypeParam { span, name, flex })
  }

  fn parse_impl_param(&mut self) -> Result<ImplParam, Diag> {
    let span = self.start_span();
    let (name, trait_) = if self.check(Token::Ident) {
      let path = self.parse_path()?;
      if path.as_ident().is_some() && self.eat(Token::Colon)? {
        let trait_ = self.parse_trait()?;
        (path.as_ident(), trait_)
      } else {
        (None, Trait { span: path.span, kind: Box::new(TraitKind::Path(path)) })
      }
    } else {
      (None, self.parse_trait()?)
    };
    let span = self.end_span(span);
    Ok(ImplParam { span, name, trait_ })
  }

  pub(crate) fn parse_generic_args(&mut self) -> Result<GenericArgs, Diag> {
    self.parse_generics(false, Self::parse_ty, Self::parse_impl)
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
    self.fmt_generics(generics, |p| self.fmt_type_param(p), |p| self.fmt_impl_param(p))
  }

  pub(crate) fn fmt_generic_args(&self, generics: &GenericArgs) -> Doc<'src> {
    self.fmt_generics(generics, |t| self.fmt_ty(t), |p| self.fmt_impl(p))
  }

  fn fmt_type_param(&self, param: &TypeParam) -> Doc<'src> {
    Doc::concat([Doc(param.name.clone()), self.fmt_flex(param.flex)])
  }

  fn fmt_impl_param(&self, param: &ImplParam) -> Doc<'src> {
    match param.name.clone() {
      Some(name) => Doc::concat([Doc(name), Doc(": "), self.fmt_trait(&param.trait_)]),
      None => self.fmt_trait(&param.trait_),
    }
  }

  pub(crate) fn fmt_generics<T, I>(
    &self,
    generics: &Generics<T, I>,
    fmt_t: impl Fn(&T) -> Doc<'src>,
    fmt_i: impl Fn(&I) -> Doc<'src>,
  ) -> Doc<'src> {
    let include_types = !generics.types.is_empty() || !generics.impls.is_empty();
    let include_impls = !generics.impls.is_empty();
    let sections = [
      include_types.then(|| self.fmt_generics_section(&generics.types, fmt_t)),
      include_impls.then(|| self.fmt_generics_section(&generics.impls, fmt_i)),
    ];
    let has_sections = sections.iter().any(|x| x.is_some());
    if !generics.inherit && !has_sections {
      Doc::EMPTY
    } else {
      Doc::concat([
        Doc("["),
        Doc::group([
          if generics.inherit {
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
    self._chart_generics(def, parent, generics, impl_allowed, Flex::None)
  }

  pub(crate) fn _chart_generics(
    &mut self,
    def: DefId,
    parent: GenericsId,
    generics: GenericParams,
    impl_allowed: bool,
    global_flex: Flex,
  ) -> GenericsId {
    self.chart.generics.push(GenericsDef {
      span: generics.span,
      def,
      parent: generics.inherit.then_some(parent),
      type_params: generics.types,
      impl_params: generics.impls,
      impl_allowed,
      global_flex,
      trait_: None,
    })
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_type_params(&mut self, generics_id: GenericsId) {
    let generics_def = &self.chart.generics[generics_id];
    let mut type_params =
      generics_def.parent.map(|id| self.sigs.type_params[id].clone()).unwrap_or_default();
    let base_index = type_params.params.len();
    for param in &generics_def.type_params {
      if let Some(&index) = type_params.lookup.get(&param.name)
        && index < base_index
      {
        continue;
      }
      let index = type_params.params.len();
      type_params.params.push(param.name.clone());
      if type_params.lookup.insert(param.name.clone(), index).is_some() {
        self.diags.report(Diag::DuplicateTypeParam { span: param.span });
      }
    }
    self.sigs.type_params.push_to(generics_id, type_params);
  }

  pub(crate) fn resolve_impl_params(&mut self, generics_id: GenericsId) {
    let generics_def = &self.chart.generics[generics_id];
    self.initialize(generics_def.def, generics_id);
    let mut impl_params =
      generics_def.parent.map(|id| self.sigs.impl_params[id].clone()).unwrap_or_default();
    self.types = take(&mut impl_params.types.types);

    if let Some(trait_id) = generics_def.trait_ {
      let parent = generics_def.parent.unwrap();
      let type_params = self.sigs.type_params[parent]
        .params
        .iter()
        .enumerate()
        .map(|(index, name)| self.types.new(TypeKind::Param(index, name.clone())))
        .collect();
      impl_params.types.inner.push(ImplType::Trait(trait_id, type_params));
    }

    for param in &generics_def.type_params {
      let index = self.sigs.type_params[generics_id].lookup[&param.name];
      let span = param.span;
      let ty = self.types.new(TypeKind::Param(index, param.name.clone()));
      if param.flex.fork() {
        impl_params.types.inner.push(if let Some(fork) = self.chart.builtins.fork {
          ImplType::Trait(fork, vec![ty])
        } else {
          ImplType::Error(self.diags.report(Diag::MissingBuiltin { span, builtin: "Fork" }))
        });
      }
      if param.flex.drop() {
        impl_params.types.inner.push(if let Some(drop) = self.chart.builtins.drop {
          ImplType::Trait(drop, vec![ty])
        } else {
          ImplType::Error(self.diags.report(Diag::MissingBuiltin { span, builtin: "Drop" }))
        });
      }
    }

    if generics_def.global_flex != Flex::None {
      for (index, name) in self.sigs.type_params[generics_id].params.iter().enumerate() {
        let ty = self.types.new(TypeKind::Param(index, name.clone()));
        if generics_def.global_flex.fork()
          && let Some(fork) = self.chart.builtins.fork
        {
          impl_params.types.inner.push(ImplType::Trait(fork, vec![ty]));
        }
        if generics_def.global_flex.drop()
          && let Some(drop) = self.chart.builtins.drop
        {
          impl_params.types.inner.push(ImplType::Trait(drop, vec![ty]));
        }
      }
    }

    for param in &generics_def.impl_params {
      let index = impl_params.types.inner.len();
      impl_params.types.inner.push(self.resolve_trait(&param.trait_));
      if let Some(name) = param.name.clone()
        && impl_params.lookup.insert(name, index).is_some()
      {
        self.diags.report(Diag::DuplicateImplParam { span: param.span });
      }
    }

    if !generics_def.impl_allowed && !impl_params.types.inner.is_empty() {
      impl_params = ImplParams::default();
      self.diags.report(Diag::UnexpectedImplParam { span: generics_def.span });
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
    inferred_type_params: Option<Vec<Type>>,
  ) -> (Vec<Type>, Vec<TirImpl>) {
    let _args = GenericArgs::empty(span);
    let args = args.unwrap_or(&_args);
    let params = &self.chart.generics[generics_id];
    let mut check_count = |got, expected, kind| {
      if got != expected {
        self.diags.report(Diag::BadGenericCount {
          span,
          path: self.chart.defs[params.def].path.clone(),
          expected,
          got,
          kind,
        });
      }
    };
    let type_param_count = self.sigs.type_params[generics_id].params.len();
    let impl_param_count =
      // Things with no inference cannot have implementation parameters; skip
      // checking the signature as this may not have been resolved yet.
      if !inference { 0 } else { self.sigs.impl_params[generics_id].types.inner.len() };
    if !inference || !args.types.is_empty() {
      check_count(args.types.len(), type_param_count, "type");
    }
    if !args.impls.is_empty() {
      check_count(args.impls.len(), impl_param_count, "impl");
    }
    let has_impl_params = impl_param_count != 0;
    let type_params = if let Some(mut inferred) = inferred_type_params {
      for (inferred, ty) in inferred.iter_mut().zip(args.types.iter()) {
        let ty = self.resolve_ty(ty, inference);
        _ = self.types.unify(*inferred, ty);
        *inferred = ty;
      }
      inferred
    } else {
      (0..type_param_count)
        .iter()
        .map(|i| {
          args.types.get(i).map(|t| self.resolve_ty(t, inference)).unwrap_or_else(|| {
            if inference {
              self.types.new_var(args.span)
            } else {
              self.types.error(ErrorGuaranteed::new_unchecked())
            }
          })
        })
        .collect::<Vec<_>>()
    };
    let impl_params = if has_impl_params {
      let impl_params_types =
        self.types.import(&self.sigs.impl_params[generics_id].types, Some(&type_params));
      if args.impls.is_empty() {
        impl_params_types.into_iter().map(|ty| self.find_impl(args.span, &ty, false)).collect()
      } else {
        impl_params_types
          .into_iter()
          .enumerate()
          .map(|(i, ty)| match args.impls.get(i) {
            Some(impl_) => self.resolve_impl_type(impl_, &ty),
            None => TirImpl::Error(ErrorGuaranteed::new_unchecked()),
          })
          .collect()
      }
    } else {
      Vec::new()
    };
    (type_params, impl_params)
  }
}
