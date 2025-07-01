use std::mem::take;

use vine_util::{idx::RangeExt, parser::Parser};

use crate::{
  components::{charter::Charter, lexer::Token, parser::VineParser, resolver::Resolver},
  structures::{
    ast::{
      GenericArgs, GenericParams, Generics, ImplParam, Path, Span, Trait, TraitKind, TypeParam,
    },
    chart::{DefId, GenericsDef, GenericsId},
    diag::{Diag, ErrorGuaranteed},
    signatures::GenericsSig,
    tir::TirImpl,
    types::{ImplType, Type, TypeCtx, TypeKind},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_generic_params(&mut self) -> Result<GenericParams<'core>, Diag<'core>> {
    self.parse_generics(Self::parse_type_param, Self::parse_impl_param)
  }

  fn parse_type_param(&mut self) -> Result<TypeParam<'core>, Diag<'core>> {
    let span = self.start_span();
    let name = self.parse_ident()?;
    let flex = self.parse_flex()?;
    let span = self.end_span(span);
    Ok(TypeParam { span, name, flex })
  }

  fn parse_impl_param(&mut self) -> Result<ImplParam<'core>, Diag<'core>> {
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

  pub(crate) fn parse_generic_args(&mut self) -> Result<GenericArgs<'core>, Diag<'core>> {
    self.parse_generics(Self::parse_ty, Self::parse_impl)
  }

  fn parse_generics<T, I>(
    &mut self,
    mut parse_t: impl FnMut(&mut Self) -> Result<T, Diag<'core>>,
    mut parse_i: impl FnMut(&mut Self) -> Result<I, Diag<'core>>,
  ) -> Result<Generics<T, I>, Diag<'core>> {
    let span = self.start_span();
    let mut types = Vec::new();
    let mut impls = Vec::new();
    if self.eat(Token::OpenBracket)? {
      loop {
        if self.eat(Token::Semi)? || self.check(Token::CloseBracket) {
          break;
        }
        types.push(parse_t(self)?);
        if self.eat(Token::Comma)? {
          continue;
        }
        if self.eat(Token::Semi)? {
          break;
        }
        if !self.check(Token::CloseBracket) {
          self.unexpected()?;
        }
      }
      loop {
        if self.eat(Token::CloseBracket)? {
          break;
        }
        impls.push(parse_i(self)?);
        if self.eat(Token::Comma)? {
          continue;
        }
        self.expect(Token::CloseBracket)?;
        break;
      }
    }
    let span = self.end_span(span);
    Ok(Generics { span, types, impls })
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_generic_params(&self, generics: &GenericParams<'core>) -> Doc<'src> {
    self.fmt_generics(generics, |p| self.fmt_type_param(p), |p| self.fmt_impl_param(p))
  }

  pub(crate) fn fmt_generic_args(&self, generics: &GenericArgs<'core>) -> Doc<'src> {
    self.fmt_generics(generics, |t| self.fmt_ty(t), |p| self.fmt_impl(p))
  }

  fn fmt_type_param(&self, param: &TypeParam<'core>) -> Doc<'src> {
    Doc::concat([Doc(param.name), self.fmt_flex(param.flex)])
  }

  fn fmt_impl_param(&self, param: &ImplParam<'core>) -> Doc<'src> {
    match param.name {
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
    if generics.impls.is_empty() && generics.types.is_empty() {
      Doc::EMPTY
    } else {
      let trailing = || Doc::if_multi(",");
      let sep = || Doc::concat([Doc(","), Doc::soft_line(" ")]);
      Doc::concat([
        Doc("["),
        if generics.types.is_empty() {
          Doc::EMPTY
        } else {
          Doc::group([Doc::interleave(generics.types.iter().map(fmt_t), sep()), trailing()])
        },
        if generics.impls.is_empty() {
          Doc::EMPTY
        } else {
          Doc::concat([
            Doc(";"),
            Doc::group([
              Doc::if_single(" "),
              Doc::interleave(generics.impls.iter().map(fmt_i), sep()),
              trailing(),
            ]),
          ])
        },
        Doc("]"),
      ])
    }
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_generics(
    &mut self,
    def: DefId,
    mut generics: GenericParams<'core>,
    impl_allowed: bool,
  ) -> GenericsId {
    if !impl_allowed && !generics.impls.is_empty() {
      self.core.report(Diag::UnexpectedImplParam { span: generics.span });
      generics.impls.clear();
    }
    self.chart.generics.push(GenericsDef {
      span: generics.span,
      def,
      type_params: generics.types,
      impl_params: generics.impls,
    })
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_generics_sig(&mut self, generics_id: GenericsId) {
    let generics_def = &self.chart.generics[generics_id];
    self.initialize(generics_def.def, generics_id);
    let mut impl_params = vec![];
    impl_params.extend(
      generics_def
        .type_params
        .iter()
        .enumerate()
        .flat_map(|(i, param)| {
          let span = param.span;
          let ty = self.types.new(TypeKind::Param(i, param.name));
          [
            param.flex.fork().then(|| {
              if let Some(fork) = self.chart.builtins.fork {
                ImplType::Trait(fork, vec![ty])
              } else {
                ImplType::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "Fork" }))
              }
            }),
            param.flex.drop().then(|| {
              if let Some(drop) = self.chart.builtins.drop {
                ImplType::Trait(drop, vec![ty])
              } else {
                ImplType::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "Drop" }))
              }
            }),
          ]
        })
        .flatten(),
    );
    impl_params.extend(generics_def.impl_params.iter().map(|p| self.resolve_trait(&p.trait_)));
    let types = take(&mut self.types);
    self.sigs.generics.push_to(generics_id, TypeCtx { types, inner: GenericsSig { impl_params } });
    if self.type_param_lookup.len() != generics_def.type_params.len() {
      self.core.report(Diag::DuplicateTypeParam { span: generics_def.span });
    }
    if self.impl_param_lookup.len()
      != generics_def.impl_params.iter().filter(|x| x.name.is_some()).count()
    {
      self.core.report(Diag::DuplicateImplParam { span: generics_def.span });
    }
  }

  pub fn resolve_generics(
    &mut self,
    path: &Path<'core>,
    params_id: GenericsId,
    inference: bool,
  ) -> (Vec<Type>, Vec<TirImpl>) {
    self._resolve_generics(path.span, path.generics.as_ref(), params_id, inference, None)
  }

  pub(crate) fn _resolve_generics(
    &mut self,
    span: Span,
    args: Option<&GenericArgs<'core>>,
    params_id: GenericsId,
    inference: bool,
    inferred_type_params: Option<Vec<Type>>,
  ) -> (Vec<Type>, Vec<TirImpl>) {
    let _args = GenericArgs { span, types: Vec::new(), impls: Vec::new() };
    let args = args.unwrap_or(&_args);
    let params = &self.chart.generics[params_id];
    let check_count = |got, expected, kind| {
      if got != expected {
        self.core.report(Diag::BadGenericCount {
          span,
          path: self.chart.defs[params.def].path,
          expected,
          got,
          kind,
        });
      }
    };
    if !inference || !args.types.is_empty() {
      check_count(args.types.len(), params.type_params.len(), "type");
    }
    let impl_param_count =
      // Things with no inference cannot have implementation parameters; skip
      // checking the `GenericsSig` as this may not have been resolved yet.
      if !inference { 0 } else { self.sigs.generics[params_id].inner.impl_params.len() };
    if !args.impls.is_empty() {
      check_count(args.impls.len(), impl_param_count, "impl");
    }
    let has_impl_params = impl_param_count != 0;
    let type_param_count = params.type_params.len();
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
        self.types.import(&self.sigs.generics[params_id], Some(&type_params)).impl_params;
      if args.impls.is_empty() {
        impl_params_types.into_iter().map(|ty| self.find_impl(args.span, &ty)).collect()
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
