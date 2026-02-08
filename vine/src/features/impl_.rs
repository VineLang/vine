use std::mem::take;

use vine_util::{idx::IdxVec, parser::Parse};

use crate::{
  components::{
    charter::{ChartedItem, Charter},
    lexer::Token,
    parser::{BRACE, Parser},
    resolver::Resolver,
  },
  structures::{
    ast::{Attr, ConstItem, Flex, FnItem, ImplItem, ImplItemKind, ItemKind, Path, Span, Vis},
    chart::{
      Binding, ConcreteConstDef, ConcreteConstId, ConcreteFnDef, ConcreteFnId, ConstId, DefId,
      DefImplKind, DefTraitKind, DefTypeKind, DefValueKind, DeriveKind, FnId, GenericsDef,
      GenericsId, GenericsKind, ImplDef, ImplDefKind, ImplId, ImplSubitem, ImplSubitemKind,
      TraitConstId, TraitFnId, TraitId, VisId,
    },
    diag::{Diag, ErrorGuaranteed},
    resolutions::{Become, ResolvedImpl, ResolvedImplKind},
    signatures::ImplSig,
    tir::TirImpl,
    types::{ImplType, Type, TypeCtx, TypeKind},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_impl_item(&mut self) -> Result<(Span, ItemKind), Diag> {
    self.expect(Token::Impl)?;
    let name_span = self.span();
    let name = self.check_then(Token::Ident, Self::parse_ident)?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::Colon)?;
    let safe = self.eat(Token::Safe)?;
    let trait_ = self.parse_trait()?;
    let kind = if self.check(Token::OpenBrace) {
      let span = self.start_span();
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      let span = self.end_span(span);
      ImplItemKind::Direct(span, items)
    } else {
      let impl_ = self.eat_then(Token::Eq, Self::parse_impl)?;
      self.expect(Token::Semi)?;
      ImplItemKind::Indirect(impl_)
    };
    Ok((name_span, ItemKind::Impl(ImplItem { name, generics, safe, trait_, kind })))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_impl_item(&self, i: &ImplItem) -> Doc<'src> {
    Doc::concat([
      Doc("impl "),
      if let Some(name) = i.name.clone() { Doc(name) } else { Doc::EMPTY },
      self.fmt_generic_params(&i.generics),
      Doc(": "),
      if i.safe { Doc("safe ") } else { Doc::EMPTY },
      self.fmt_trait(&i.trait_),
      match &i.kind {
        ImplItemKind::Direct(span, items) => Doc::concat([
          Doc(" "),
          self.fmt_block_like(*span, items.iter().map(|i| (i.span, self.fmt_item(i)))),
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
  ) -> ChartedItem {
    let def = if let Some(name) = impl_item.name.clone() {
      self.chart_child(parent, span, name, member_vis, true)
    } else {
      parent
    };
    let generics = self.chart_generics(def, parent_generics, impl_item.generics, true);
    let mut subitems = Vec::new();
    let kind = match impl_item.kind {
      ImplItemKind::Direct(_, items) => {
        for subitem in items {
          let span = subitem.name_span;
          if !matches!(subitem.vis, Vis::Private) {
            self.diags.error(Diag::ImplItemVis { span });
          }
          match subitem.kind {
            ItemKind::Const(const_item) => {
              subitems.push(self.chart_impl_const(
                def,
                generics,
                span,
                subitem.attrs,
                subitem.unsafe_,
                const_item,
              ));
            }
            ItemKind::Fn(fn_item) => subitems.push(self.chart_impl_fn(
              def,
              generics,
              span,
              subitem.attrs,
              subitem.unsafe_,
              fn_item,
            )),
            _ => {
              self.diags.error(Diag::InvalidImplItem { span });
            }
          }
        }
        ImplDefKind::Direct(impl_item.safe, impl_item.trait_, subitems)
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
      vis,
      unsafe_: false,
    });
    self.annotations.record_reference(span, span);
    self.define_impl(span, def, vis, DefImplKind::Impl(impl_id));
    ChartedItem::Impl(def, impl_id)
  }

  fn chart_impl_fn(
    &mut self,
    parent_def: DefId,
    parent_generics: GenericsId,
    span: Span,
    attrs: Vec<Attr>,
    unsafe_: bool,
    mut fn_item: FnItem,
  ) -> ImplSubitem {
    if fn_item.method {
      self.diags.error(Diag::ImplItemMethod { span });
    }
    if fn_item.generics.inherit {
      self.diags.error(Diag::ImplItemInheritGen { span });
    }
    fn_item.generics.inherit = true;
    let vis = VisId::Def(parent_def);
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
      unsafe_,
    });
    self.define_value(span, def, vis, DefValueKind::Fn(FnId::Concrete(fn_id)));
    let charted_item = ChartedItem::Fn(def, FnId::Concrete(fn_id));
    self.chart_attrs(vis, charted_item, attrs);
    self.annotations.record_reference(span, span);
    ImplSubitem { span, name: fn_item.name, kind: ImplSubitemKind::Fn(fn_id) }
  }

  fn chart_impl_const(
    &mut self,
    parent_def: DefId,
    parent_generics: GenericsId,
    span: Span,
    attrs: Vec<Attr>,
    unsafe_: bool,
    mut const_item: ConstItem,
  ) -> ImplSubitem {
    if const_item.generics.inherit {
      self.diags.error(Diag::ImplItemInheritGen { span });
    }
    const_item.generics.inherit = true;
    let vis = VisId::Def(parent_def);
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
      unsafe_,
      configurable: false,
    });
    self.define_value(span, def, vis, DefValueKind::Const(ConstId::Concrete(const_id)));
    let charted_item = ChartedItem::Const(def, ConstId::Concrete(const_id));
    self.chart_attrs(vis, charted_item, attrs);
    if unsafe_ {
      self.chart_unsafe(span, charted_item);
    }
    self.annotations.record_reference(span, span);
    ImplSubitem { span, name: const_item.name, kind: ImplSubitemKind::Const(const_id) }
  }

  pub fn chart_flex_impls(
    &mut self,
    def: DefId,
    ty_generics: GenericsId,
    vis: VisId,
    ty: DefTypeKind,
    span: Span,
    flex: Flex,
  ) {
    if flex.fork() {
      self.chart_derive_impl(def, ty_generics, vis, ty, span, DeriveKind::Fork(span));
    }
    if flex.drop() {
      self.chart_derive_impl(def, ty_generics, vis, ty, span, DeriveKind::Drop(span));
    }
  }

  pub fn chart_derive_impls(
    &mut self,
    def: DefId,
    ty_generics: GenericsId,
    vis: VisId,
    ty: DefTypeKind,
    traits: Vec<Path>,
  ) {
    for trait_ in traits {
      self.chart_derive_impl(def, ty_generics, vis, ty, trait_.span, DeriveKind::Trait(trait_));
    }
  }

  fn chart_derive_impl(
    &mut self,
    def: DefId,
    ty_generics: GenericsId,
    vis: VisId,
    ty: DefTypeKind,
    span: Span,
    kind: DeriveKind,
  ) {
    let generics = self.chart.generics.push(GenericsDef {
      span,
      def,
      parent: Some(ty_generics),
      impl_allowed: true,
      kind: GenericsKind::Derive(kind.clone()),
    });
    let impl_ = self.chart.impls.push(ImplDef {
      span,
      def,
      name: None,
      generics,
      kind: ImplDefKind::Derive(kind, ty),
      manual: false,
      basic: false,
      become_: None,
      vis,
      unsafe_: false,
    });
    self.define_impl(span, def, vis, DefImplKind::Impl(impl_));
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_impl_sig(&mut self, impl_id: ImplId) {
    let impl_def = &self.chart.impls[impl_id];
    let span = impl_def.span;
    self.initialize(impl_def.def, impl_def.generics, false);
    let ty = match &impl_def.kind {
      ImplDefKind::Direct(_, trait_, _) | ImplDefKind::Indirect(trait_, _) => {
        self.resolve_trait(trait_)
      }
      ImplDefKind::Derive(kind, ty) => self.resolve_derive_impl_ty(kind, *ty),
    };

    let hover = format!(
      "impl {}{}: {};",
      match &impl_def.name {
        Some(name) => &name.0,
        None => "",
      },
      self.show_generics(self.cur_generics, true),
      self.types.show_impl_type(self.chart, &ty)
    );

    let types = take(&mut self.types);
    self.annotations.record_signature(impl_def.span, hover);
    if let ImplType::Trait(trait_id, _) = ty {
      let old = self
        .sigs
        .def_impls
        .get_or_extend(impl_def.def)
        .insert(trait_id, Binding { span, vis: impl_def.vis, kind: DefImplKind::Impl(impl_id) });
      if old.is_some() {
        self.diags.error(Diag::DuplicateImpl {
          span,
          path: self.chart.defs[impl_def.def].path.clone(),
          trait_: self.chart.traits[trait_id].name.clone(),
        });
      }
    }
    self.sigs.impls.push_to(impl_id, TypeCtx { types, inner: ImplSig { ty } });
  }

  pub(crate) fn resolve_derive_kind(&mut self, kind: &DeriveKind) -> Result<TraitId, Diag> {
    match kind {
      DeriveKind::Fork(span) => {
        self.chart.builtins.fork.ok_or(Diag::MissingBuiltin { span: *span, builtin: "Fork" })
      }
      DeriveKind::Drop(span) => {
        self.chart.builtins.drop.ok_or(Diag::MissingBuiltin { span: *span, builtin: "Drop" })
      }
      DeriveKind::Trait(trait_) => {
        let span = trait_.span;
        if trait_.generics.is_some() {
          return Err(Diag::DerivePassedArguments { span });
        }
        match self.resolve_path(self.cur_def, trait_, "trait", |d| d.trait_kind) {
          Ok(DefTraitKind::Trait(trait_id)) => {
            if self.sigs.type_params[self.chart.traits[trait_id].generics].params.len() == 1 {
              Ok(trait_id)
            } else {
              Err(Diag::DeriveMultiParams { span })
            }
          }
          Err(diag) => Err(diag),
        }
      }
    }
  }

  fn resolve_derive_impl_ty(&mut self, kind: &DeriveKind, ty: DefTypeKind) -> ImplType {
    match self.resolve_derive_kind(kind) {
      Ok(trait_id) => {
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
            DefTypeKind::Struct(struct_id) => self.types.new_struct(self.chart, struct_id, params),
            DefTypeKind::Enum(enum_id) => self.types.new(TypeKind::Enum(enum_id, params)),
            _ => unreachable!(),
          }],
        )
      }
      Err(diag) => ImplType::Error(self.diags.error(diag)),
    }
  }

  pub(crate) fn resolve_impl_def(&mut self, impl_id: ImplId) {
    let impl_def = &self.chart.impls[impl_id];
    self.initialize(impl_def.def, impl_def.generics, impl_def.unsafe_);
    let span = impl_def.span;
    let ty = self.types.import(&self.sigs.impls[impl_id], None).ty;
    let resolved = match &ty {
      ImplType::Trait(trait_id, type_params) => {
        let kind = match &impl_def.kind {
          ImplDefKind::Direct(safe, _, subitems) => {
            if self.chart.traits[*trait_id].unsafe_ && !safe && !impl_def.unsafe_ {
              self.diags.error(Diag::Unsafe { span });
            }
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
                  .error(Diag::ExtraneousImplItem { span: item.span, name: item.name.clone() });
              }
            }
            ResolvedImplKind::Direct { fns, consts }
          }
          ImplDefKind::Indirect(_, Some(impl_)) => {
            ResolvedImplKind::Indirect(self.resolve_impl_type(impl_, &ty))
          }
          ImplDefKind::Indirect(_, None) | ImplDefKind::Derive(..) => {
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
        self.diags.error(diag);
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
          self.resolve_path_impl(impl_def.def, become_path, trait_id)?;
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
      return Err(self.diags.error(Diag::IncompleteImpl { span, name }));
    };
    let span = subitem.span;
    let ImplSubitemKind::Fn(fn_id) = subitem.kind else {
      return Err(self.diags.error(Diag::WrongImplSubitemKind { span, expected: "fn" }));
    };
    let impl_generics = self.chart.concrete_fns[fn_id].generics;
    let type_params =
      self.resolve_impl_subitem_generics(span, impl_generics, trait_generics, type_params)?;
    let expected =
      self.types.import(&self.sigs.traits[trait_id].fns[trait_fn_id], Some(&type_params));
    let found = self.types.import(&self.sigs.concrete_fns[fn_id], None);
    Self::expect_fn_sig(self.diags, self.chart, &mut self.types, span, expected, found);
    if !self.allow_unsafe && !trait_fn.unsafe_ && self.chart.concrete_fns[fn_id].unsafe_ {
      self.diags.error(Diag::ImplExpectedSafe { span, kind: "fn" });
    }
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
      return Err(self.diags.error(Diag::IncompleteImpl { span, name }));
    };
    let span = subitem.span;
    let ImplSubitemKind::Const(const_id) = subitem.kind else {
      return Err(self.diags.error(Diag::WrongImplSubitemKind { span, expected: "const" }));
    };
    let impl_generics = self.chart.concrete_consts[const_id].generics;
    let type_params =
      self.resolve_impl_subitem_generics(span, impl_generics, trait_generics, type_params)?;
    let expected_ty =
      self.types.import(&self.sigs.traits[trait_id].consts[trait_const_id], Some(&type_params)).ty;
    let const_sig = &self.sigs.concrete_consts[const_id];
    let found_ty = self.types.import(const_sig, None).ty;
    if self.types.unify(expected_ty, found_ty).is_failure() {
      self.diags.error(Diag::ExpectedTypeFound {
        span,
        expected: self.types.show(self.chart, expected_ty),
        found: self.types.show(self.chart, found_ty),
      });
    }
    if !self.allow_unsafe && !trait_const.unsafe_ && self.chart.concrete_consts[const_id].unsafe_ {
      self.diags.error(Diag::ImplExpectedSafe { span, kind: "const" });
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
      Err(self.diags.error(Diag::ExpectedImplItemTypeParams { span, expected, found }))?;
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
      Err(self.diags.error(Diag::ExpectedImplItemImplParams { span, expected, found }))?;
    }

    let trait_item_params =
      self.types.import(&self.sigs.impl_params[trait_item_generics].types, Some(&type_params));
    let impl_item_params =
      self.types.import(&self.sigs.impl_params[impl_item_generics].types, None);
    for (expected, found) in
      trait_item_params[trait_params..].iter().zip(&impl_item_params[impl_params..])
    {
      if self.types.unify_impl_type(expected, found).is_failure() {
        Err(self.diags.error(Diag::ExpectedImplItemImplParam {
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
    let span = path.span;
    if self.chart.impls[impl_id].unsafe_ {
      self.unsafe_(span);
    }
    let generics_id = self.chart.impls[impl_id].generics;
    let type_params = self.types.new_vars(span, self.sigs.type_params[generics_id].params.len());
    let actual_ty = self.types.import(&self.sigs.impls[impl_id], Some(&type_params)).ty;
    // just need inference; errors will be reported later
    _ = self.types.unify_impl_type(&actual_ty, ty);
    let (type_params, impl_params) =
      self._resolve_generics(span, path.generics.as_ref(), generics_id, true, Some(type_params));
    let actual_ty = self.types.import(&self.sigs.impls[impl_id], Some(&type_params)).ty;
    if self.types.unify_impl_type(&actual_ty, ty).is_failure() {
      self.diags.error(Diag::ExpectedTypeFound {
        span,
        expected: self.types.show_impl_type(self.chart, ty),
        found: self.types.show_impl_type(self.chart, &actual_ty),
      });
    }
    TirImpl::Def(impl_id, impl_params)
  }
}
