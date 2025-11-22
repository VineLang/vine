use std::mem::take;

use vine_util::{idx::IdxVec, parser::Parser};

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{BRACE, VineParser},
    resolver::Resolver,
  },
  structures::{
    ast::{
      Attr, ConstItem, Flex, FnItem, GenericParams, Ident, ImplItem, ImplItemKind, ItemKind, Path,
      Span, Vis,
    },
    chart::{
      ConcreteConstDef, ConcreteConstId, ConcreteFnDef, ConcreteFnId, ConstId, DefId, DefImplKind,
      DefTypeKind, DefValueKind, FnId, GenericsId, ImplDef, ImplDefKind, ImplId, ImplSubitem,
      ImplSubitemKind, TraitConstId, TraitFnId, TraitId, VisId,
    },
    diag::{Diag, ErrorGuaranteed},
    resolutions::{Become, ResolvedImpl, ResolvedImplKind},
    signatures::ImplSig,
    tir::TirImpl,
    types::{ImplType, Type, TypeCtx, TypeKind},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl VineParser<'_> {
  pub(crate) fn parse_impl_item(&mut self) -> Result<(Span, ItemKind), Diag> {
    self.expect(Token::Impl)?;
    let name_span = self.span();
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::Colon)?;
    let trait_ = self.parse_trait()?;
    let kind = if self.check(Token::OpenBrace) {
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      ImplItemKind::Direct(items)
    } else {
      let impl_ = self.eat_then(Token::Eq, Self::parse_impl)?;
      self.expect(Token::Semi)?;
      ImplItemKind::Indirect(impl_)
    };
    Ok((name_span, ItemKind::Impl(ImplItem { name, generics, trait_, kind })))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_impl_item(&self, span: Span, i: &ImplItem) -> Doc<'src> {
    Doc::concat([
      Doc("impl "),
      Doc(i.name.clone()),
      self.fmt_generic_params(&i.generics),
      Doc(": "),
      self.fmt_trait(&i.trait_),
      match &i.kind {
        ImplItemKind::Direct(items) => Doc::concat([
          Doc(" "),
          self.fmt_block_like(span, items.iter().map(|i| (i.span, self.fmt_item(i)))),
        ]),
        ImplItemKind::Indirect(Some(impl_)) => {
          Doc::concat([Doc(" = "), self.fmt_impl(impl_), Doc(";")])
        }
        ImplItemKind::Indirect(None) => Doc(";"),
      },
    ])
  }
}

impl Charter<'_> {
  pub(crate) fn chart_impl(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: VisId,
    member_vis: VisId,
    impl_item: ImplItem,
  ) -> DefId {
    let def = self.chart_child(parent, span, impl_item.name.clone(), member_vis, true);
    let generics = self.chart_generics(def, parent_generics, impl_item.generics, true);
    let mut subitems = Vec::new();
    let kind = match impl_item.kind {
      ImplItemKind::Direct(items) => {
        for subitem in items {
          if !self.enabled(&subitem.attrs) {
            continue;
          }
          let span = subitem.name_span;
          if !matches!(subitem.vis, Vis::Private) {
            self.diags.report(Diag::ImplItemVis { span });
          }
          match subitem.kind {
            ItemKind::Const(const_item) => subitems.push(self.chart_impl_const(
              vis,
              def,
              generics,
              span,
              subitem.attrs,
              const_item,
            )),
            ItemKind::Fn(fn_item) => {
              subitems.push(self.chart_impl_fn(vis, def, generics, span, subitem.attrs, fn_item))
            }
            _ => {
              self.diags.report(Diag::InvalidImplItem { span });
            }
          }
        }
        ImplDefKind::Direct(impl_item.trait_, subitems)
      }
      ImplItemKind::Indirect(impl_) => ImplDefKind::Indirect(impl_item.trait_, impl_),
    };
    let impl_id = self.chart.impls.push(ImplDef {
      span,
      name: impl_item.name,
      def,
      generics,
      kind,
      manual: false,
      basic: false,
      become_: None,
    });
    self.define_impl(span, def, vis, DefImplKind::Impl(impl_id));
    def
  }

  fn chart_impl_fn(
    &mut self,
    vis: VisId,
    parent_def: DefId,
    parent_generics: GenericsId,
    span: Span,
    attrs: Vec<Attr>,
    mut fn_item: FnItem,
  ) -> ImplSubitem {
    if fn_item.method {
      self.diags.report(Diag::ImplItemMethod { span });
    }
    if fn_item.generics.inherit {
      self.diags.report(Diag::ImplItemInheritGen { span });
    }
    fn_item.generics.inherit = true;
    let def = self.chart_child(parent_def, span, fn_item.name.clone(), vis, false);
    let generics = self.chart_generics(def, parent_generics, fn_item.generics, true);
    let body = self.ensure_implemented(span, fn_item.body);
    let fn_id = self.chart.concrete_fns.push(ConcreteFnDef {
      span,
      def,
      name: fn_item.name.clone(),
      generics,
      method: fn_item.method,
      params: fn_item.params,
      ret_ty: fn_item.ret,
      body,
      frameless: false,
    });
    self.define_value(span, def, vis, DefValueKind::Fn(FnId::Concrete(fn_id)));
    self.chart_attrs(Some(def), attrs);
    ImplSubitem { span, name: fn_item.name, kind: ImplSubitemKind::Fn(fn_id) }
  }

  fn chart_impl_const(
    &mut self,
    vis: VisId,
    parent_def: DefId,
    parent_generics: GenericsId,
    span: Span,
    attrs: Vec<Attr>,
    mut const_item: ConstItem,
  ) -> ImplSubitem {
    if const_item.generics.inherit {
      self.diags.report(Diag::ImplItemInheritGen { span });
    }
    const_item.generics.inherit = true;
    let def = self.chart_child(parent_def, span, const_item.name.clone(), vis, false);
    let generics = self.chart_generics(def, parent_generics, const_item.generics, true);
    let value = self.ensure_implemented(span, const_item.value);
    let const_id = self.chart.concrete_consts.push(ConcreteConstDef {
      span,
      def,
      name: const_item.name.clone(),
      generics,
      ty: const_item.ty,
      value,
    });
    self.define_value(span, def, vis, DefValueKind::Const(ConstId::Concrete(const_id)));
    self.chart_attrs(Some(def), attrs);
    ImplSubitem { span, name: const_item.name, kind: ImplSubitemKind::Const(const_id) }
  }

  pub fn chart_flex_impls(
    &mut self,
    ty_def: DefId,
    ty_generics: GenericsId,
    span: Span,
    vis: VisId,
    member_vis: VisId,
    ty: DefTypeKind,
    flex: Flex,
  ) {
    if flex.fork() {
      let name = Ident("fork".into());
      let def = self.chart_child(ty_def, span, name.clone(), member_vis, false);
      let _generic_params = GenericParams { inherit: true, ..GenericParams::empty(span) };
      let generics = self._chart_generics(def, ty_generics, _generic_params, true, Flex::Fork);
      let impl_ = self.chart.impls.push(ImplDef {
        span,
        name,
        def,
        generics,
        kind: ImplDefKind::IndirectFork(ty),
        manual: false,
        basic: false,
        become_: None,
      });
      self.define_impl(span, def, vis, DefImplKind::Impl(impl_));
    }
    if flex.drop() {
      let name = Ident("drop".into());
      let def = self.chart_child(ty_def, span, name.clone(), member_vis, false);
      let _generic_params = GenericParams { inherit: true, ..GenericParams::empty(span) };
      let generics = self._chart_generics(def, ty_generics, _generic_params, true, Flex::Drop);
      let impl_ = self.chart.impls.push(ImplDef {
        span,
        name,
        def,
        generics,
        kind: ImplDefKind::IndirectDrop(ty),
        manual: false,
        basic: false,
        become_: None,
      });
      self.define_impl(span, def, vis, DefImplKind::Impl(impl_));
    }
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_impl_sig(&mut self, impl_id: ImplId) {
    let impl_def = &self.chart.impls[impl_id];
    let span = impl_def.span;
    self.initialize(impl_def.def, impl_def.generics);
    let ty = match &impl_def.kind {
      ImplDefKind::Direct(trait_, _) | ImplDefKind::Indirect(trait_, _) => {
        self.resolve_trait(trait_)
      }
      ImplDefKind::IndirectFork(ty) => {
        self.resolve_flex_impl_ty(span, *ty, self.chart.builtins.fork, "Fork")
      }
      ImplDefKind::IndirectDrop(ty) => {
        self.resolve_flex_impl_ty(span, *ty, self.chart.builtins.drop, "Drop")
      }
    };

    let hover = format!(
      "impl {}{}: {};",
      impl_def.name,
      self.show_generics(self.cur_generics, true),
      self.types.show_impl_type(self.chart, &ty)
    );

    let types = take(&mut self.types);
    self.annotations.record_hover(impl_def.span, hover);
    self.sigs.impls.push_to(impl_id, TypeCtx { types, inner: ImplSig { ty } });
  }

  fn resolve_flex_impl_ty(
    &mut self,
    span: Span,
    ty: DefTypeKind,
    trait_id: Option<TraitId>,
    builtin: &'static str,
  ) -> ImplType {
    if let Some(trait_id) = trait_id {
      let params = Vec::from_iter(
        self.sigs.type_params[self.cur_generics]
          .params
          .iter()
          .enumerate()
          .map(|(i, name)| self.types.new(TypeKind::Param(i, name.clone()))),
      );
      ImplType::Trait(
        trait_id,
        vec![match ty {
          DefTypeKind::Struct(struct_id) => self.types.new(TypeKind::Struct(struct_id, params)),
          DefTypeKind::Enum(enum_id) => self.types.new(TypeKind::Enum(enum_id, params)),
          _ => unreachable!(),
        }],
      )
    } else {
      ImplType::Error(self.diags.report(Diag::MissingBuiltin { span, builtin }))
    }
  }

  pub(crate) fn resolve_impl_def(&mut self, impl_id: ImplId) {
    let impl_def = &self.chart.impls[impl_id];
    self.initialize(impl_def.def, impl_def.generics);
    let span = impl_def.span;
    let ty = self.types.import(&self.sigs.impls[impl_id], None).ty;
    let resolved = match &ty {
      ImplType::Trait(trait_id, type_params) => {
        let kind = match &impl_def.kind {
          ImplDefKind::Direct(_, subitems) => {
            let fns = IdxVec::from_iter(self.sigs.traits[*trait_id].fns.keys().map(|fn_id| {
              self.resolve_impl_subitem_fn(span, subitems, type_params, *trait_id, fn_id)
            }));
            let consts =
              IdxVec::from_iter(self.sigs.traits[*trait_id].consts.keys().map(|const_id| {
                self.resolve_impl_subitem_const(span, subitems, type_params, *trait_id, const_id)
              }));
            let trait_def = &self.chart.traits[*trait_id];
            for item in subitems.iter() {
              if trait_def.consts.values().all(|x| x.name != item.name)
                && trait_def.fns.values().all(|x| x.name != item.name)
              {
                self
                  .diags
                  .report(Diag::ExtraneousImplItem { span: item.span, name: item.name.clone() });
              }
            }
            ResolvedImplKind::Direct { fns, consts }
          }
          ImplDefKind::Indirect(_, Some(impl_)) => {
            ResolvedImplKind::Indirect(self.resolve_impl_type(impl_, &ty))
          }
          ImplDefKind::Indirect(_, None)
          | ImplDefKind::IndirectFork(_)
          | ImplDefKind::IndirectDrop(_) => {
            ResolvedImplKind::Indirect(self.find_impl(span, &ty, true))
          }
        };
        Ok(ResolvedImpl { kind, trait_id: *trait_id, become_: Become::Unresolved })
      }
      ImplType::Error(err) => Err(*err),
    };
    self.resolutions.impls.push_to(impl_id, resolved);
  }

  pub(crate) fn resolve_impl_become(&mut self, impl_id: ImplId) -> Become {
    let impl_def = &self.chart.impls[impl_id];
    let Ok(resolved) = &mut self.resolutions.impls[impl_id] else { return Become::Resolved(None) };
    let Become::Unresolved = resolved.become_ else { return resolved.become_.clone() };
    resolved.become_ = Become::Resolving;
    let trait_id = resolved.trait_id;
    let become_ = match self._resolve_impl_become(impl_id, trait_id, impl_def) {
      Ok(become_) => Become::Resolved(become_),
      Err(diag) => {
        self.diags.report(diag);
        Become::Resolved(None)
      }
    };
    let Ok(resolved) = &mut self.resolutions.impls[impl_id] else { return Become::Resolved(None) };
    resolved.become_ = become_.clone();
    become_
  }

  fn _resolve_impl_become(
    &mut self,
    impl_id: ImplId,
    trait_id: TraitId,
    impl_def: &ImplDef,
  ) -> Result<Option<(ImplId, Vec<usize>)>, Diag> {
    let necessary_params = self.sigs.impl_params[impl_def.generics]
      .types
      .inner
      .iter()
      .enumerate()
      .filter_map(|(i, x)| matches!(x, ImplType::Trait(t, _) if t == &trait_id).then_some(i))
      .collect();
    Ok(match &impl_def.become_ {
      Some(become_path) => {
        if let Some(args) = &become_path.generics {
          Err(Diag::GenericBecomeAttr { span: args.span })?
        }
        let DefImplKind::Impl(become_id) =
          self.resolve_path(impl_def.def, become_path, "impl", |d| d.impl_kind)?;
        if self.resolutions.impls[become_id].as_ref().map_err(|&e| e)?.trait_id != trait_id {
          Err(Diag::BecomeOtherTrait { span: become_path.span })?
        }
        if !self.sigs.impl_params[self.chart.impls[become_id].generics].types.inner.is_empty() {
          Err(Diag::BecomeGenericImpl { span: become_path.span })?
        }
        Some((become_id, necessary_params))
      }
      None => match &self.resolutions.impls[impl_id].as_ref().unwrap().kind {
        ResolvedImplKind::Direct { .. } => self.sigs.impl_params[impl_def.generics]
          .types
          .inner
          .is_empty()
          .then_some((impl_id, Vec::new())),
        ResolvedImplKind::Indirect(impl_) => {
          self.resolve_tir_impl_become(&impl_.clone(), None).map(|id| (id, necessary_params))
        }
      },
    })
  }

  fn resolve_tir_impl_become(
    &mut self,
    impl_: &TirImpl,
    fallback: Option<ImplId>,
  ) -> Option<ImplId> {
    match impl_ {
      TirImpl::Def(impl_id, args) => match self.resolve_impl_become(*impl_id) {
        Become::Resolved(Some((impl_id, indices))) => indices
          .into_iter()
          .all(|i| self.resolve_tir_impl_become(&args[i], Some(impl_id)) == Some(impl_id))
          .then_some(impl_id),
        Become::Resolving => fallback,
        _ => None,
      },
      _ => None,
    }
  }

  fn resolve_impl_subitem_fn(
    &mut self,
    span: Span,
    subitems: &[ImplSubitem],
    type_params: &[Type],
    trait_id: TraitId,
    trait_fn_id: TraitFnId,
  ) -> Result<ConcreteFnId, ErrorGuaranteed> {
    let trait_fn = &self.chart.traits[trait_id].fns[trait_fn_id];
    let name = trait_fn.name.clone();
    let trait_generics = trait_fn.generics;
    let Some(subitem) = subitems.iter().find(|i| i.name == name) else {
      return Err(self.diags.report(Diag::IncompleteImpl { span, name }));
    };
    let span = subitem.span;
    let ImplSubitemKind::Fn(fn_id) = subitem.kind else {
      return Err(self.diags.report(Diag::WrongImplSubitemKind { span, expected: "fn" }));
    };
    let impl_generics = self.chart.concrete_fns[fn_id].generics;
    let type_params =
      self.resolve_impl_subitem_generics(span, impl_generics, trait_generics, type_params)?;
    let expected =
      self.types.import(&self.sigs.traits[trait_id].fns[trait_fn_id], Some(&type_params));
    let found = self.types.import(&self.sigs.concrete_fns[fn_id], None);
    Self::expect_fn_sig(self.diags, self.chart, &mut self.types, span, expected, found);
    Ok(fn_id)
  }

  fn resolve_impl_subitem_const(
    &mut self,
    span: Span,
    subitems: &[ImplSubitem],
    type_params: &[Type],
    trait_id: TraitId,
    trait_const_id: TraitConstId,
  ) -> Result<ConcreteConstId, ErrorGuaranteed> {
    let trait_const = &self.chart.traits[trait_id].consts[trait_const_id];
    let name = trait_const.name.clone();
    let trait_generics = trait_const.generics;
    let Some(subitem) = subitems.iter().find(|i| i.name == name) else {
      return Err(self.diags.report(Diag::IncompleteImpl { span, name }));
    };
    let span = subitem.span;
    let ImplSubitemKind::Const(const_id) = subitem.kind else {
      return Err(self.diags.report(Diag::WrongImplSubitemKind { span, expected: "const" }));
    };
    let impl_generics = self.chart.concrete_consts[const_id].generics;
    let type_params =
      self.resolve_impl_subitem_generics(span, impl_generics, trait_generics, type_params)?;
    let expected_ty =
      self.types.import(&self.sigs.traits[trait_id].consts[trait_const_id], Some(&type_params)).ty;
    let const_sig = &self.sigs.concrete_consts[const_id];
    let found_ty = self.types.import(const_sig, None).ty;
    if self.types.unify(expected_ty, found_ty).is_failure() {
      self.diags.report(Diag::ExpectedTypeFound {
        span,
        expected: self.types.show(self.chart, expected_ty),
        found: self.types.show(self.chart, found_ty),
      });
    }
    Ok(const_id)
  }

  fn resolve_impl_subitem_generics(
    &mut self,
    span: Span,
    impl_item_generics: GenericsId,
    trait_item_generics: GenericsId,
    type_params: &[Type],
  ) -> Result<Vec<Type>, ErrorGuaranteed> {
    let impl_generics = self.chart.generics[impl_item_generics].parent.unwrap();
    let trait_generics = self.chart.generics[trait_item_generics].parent.unwrap();

    let impl_params = self.sigs.type_params[impl_generics].params.len();
    let impl_item_params = self.sigs.type_params[impl_item_generics].params.len();
    let trait_params = self.sigs.type_params[trait_generics].params.len();
    let trait_item_params = self.sigs.type_params[trait_item_generics].params.len();

    let expected = trait_item_params - trait_params;
    let found = impl_item_params - impl_params;
    if found != expected {
      Err(self.diags.report(Diag::ExpectedImplItemTypeParams { span, expected, found }))?;
    }

    let type_params = Vec::from_iter(type_params.iter().copied().chain(
      (impl_params..impl_params + expected).map(|i| {
        self
          .types
          .new(TypeKind::Param(i, self.sigs.type_params[impl_item_generics].params[i].clone()))
      }),
    ));

    let impl_params = self.sigs.impl_params[impl_generics].types.inner.len();
    let impl_item_params = self.sigs.impl_params[impl_item_generics].types.inner.len();
    let trait_params = 1;
    let trait_item_params = self.sigs.impl_params[trait_item_generics].types.inner.len();

    let expected = trait_item_params - trait_params;
    let found = impl_item_params - impl_params;
    if found != expected {
      Err(self.diags.report(Diag::ExpectedImplItemImplParams { span, expected, found }))?;
    }

    let trait_item_params =
      self.types.import(&self.sigs.impl_params[trait_item_generics].types, Some(&type_params));
    let impl_item_params =
      self.types.import(&self.sigs.impl_params[impl_item_generics].types, None);
    for (expected, found) in
      trait_item_params[trait_params..].iter().zip(&impl_item_params[impl_params..])
    {
      if self.types.unify_impl_type(expected, found).is_failure() {
        Err(self.diags.report(Diag::ExpectedImplItemImplParam {
          span,
          expected: self.types.show_impl_type(self.chart, expected),
          found: self.types.show_impl_type(self.chart, found),
        }))?
      }
    }
    Ok(type_params)
  }

  pub(crate) fn resolve_impl_path_impl(
    &mut self,
    path: &Path,
    impl_id: ImplId,
    ty: &ImplType,
  ) -> TirImpl {
    let generics_id = self.chart.impls[impl_id].generics;
    let type_params =
      self.types.new_vars(path.span, self.sigs.type_params[generics_id].params.len());
    let actual_ty = self.types.import(&self.sigs.impls[impl_id], Some(&type_params)).ty;
    // just need inference; errors will be reported later
    _ = self.types.unify_impl_type(&actual_ty, ty);
    let (type_params, impl_params) = self._resolve_generics(
      path.span,
      path.generics.as_ref(),
      generics_id,
      true,
      Some(type_params),
    );
    let actual_ty = self.types.import(&self.sigs.impls[impl_id], Some(&type_params)).ty;
    if self.types.unify_impl_type(&actual_ty, ty).is_failure() {
      self.diags.report(Diag::ExpectedTypeFound {
        span: path.span,
        expected: self.types.show_impl_type(self.chart, ty),
        found: self.types.show_impl_type(self.chart, &actual_ty),
      });
    }
    TirImpl::Def(impl_id, impl_params)
  }
}
