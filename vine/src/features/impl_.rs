use std::mem::take;

use vine_util::{idx::IdxVec, parser::Parser};

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{VineParser, BRACE},
    resolver::Resolver,
  },
  structures::{
    ast::{Attr, ConstItem, FnItem, ImplItem, ImplItemKind, ItemKind, Path, Span, Vis},
    chart::{
      ConcreteConstDef, ConcreteConstId, ConcreteFnDef, ConcreteFnId, ConstId, DefId, DefImplKind,
      DefValueKind, FnId, GenericsId, ImplDef, ImplDefKind, ImplId, ImplSubitem, ImplSubitemKind,
      TraitConstId, TraitFnId, TraitId,
    },
    diag::{Diag, ErrorGuaranteed},
    resolutions::{Become, ResolvedImpl, ResolvedImplKind},
    signatures::ImplSig,
    tir::TirImpl,
    types::{ImplType, Type, TypeCtx, TypeKind},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_impl_item(&mut self) -> Result<ImplItem<'core>, Diag<'core>> {
    self.expect(Token::Impl)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::Colon)?;
    let trait_ = self.parse_trait()?;
    let kind = if self.eat(Token::Eq)? {
      let impl_ = self.parse_impl()?;
      self.expect(Token::Semi)?;
      ImplItemKind::Indirect(impl_)
    } else {
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      ImplItemKind::Direct(items)
    };
    Ok(ImplItem { name, generics, trait_, kind })
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_impl_item(&self, span: Span, i: &ImplItem<'core>) -> Doc<'src> {
    Doc::concat([
      Doc("impl "),
      Doc(i.name),
      self.fmt_generic_params(&i.generics),
      Doc(": "),
      self.fmt_trait(&i.trait_),
      Doc(" "),
      match &i.kind {
        ImplItemKind::Direct(items) => {
          self.fmt_block_like(span, items.iter().map(|i| (i.span, self.fmt_item(i))))
        }
        ImplItemKind::Indirect(impl_) => Doc::concat([Doc("= "), self.fmt_impl(impl_), Doc(";")]),
      },
    ])
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_impl(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: DefId,
    member_vis: DefId,
    impl_item: ImplItem<'core>,
  ) -> DefId {
    let def = self.chart_child(parent, impl_item.name, member_vis, true);
    let generics = self.chart_generics(def, parent_generics, impl_item.generics, true);
    let mut subitems = Vec::new();
    let kind = match impl_item.kind {
      ImplItemKind::Direct(items) => {
        for subitem in items {
          let span = subitem.span;
          if !matches!(subitem.vis, Vis::Private) {
            self.core.report(Diag::ImplItemVis { span });
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
              self.core.report(Diag::InvalidImplItem { span });
            }
          }
        }
        ImplDefKind::Direct(subitems)
      }
      ImplItemKind::Indirect(impl_) => ImplDefKind::Indirect(impl_),
    };
    let impl_id = self.chart.impls.push(ImplDef {
      span,
      def,
      generics,
      trait_: impl_item.trait_,
      kind,
      manual: false,
      become_: None,
    });
    self.define_impl(span, def, vis, DefImplKind::Impl(impl_id));
    def
  }

  fn chart_impl_fn(
    &mut self,
    vis: DefId,
    parent_def: DefId,
    parent_generics: GenericsId,
    span: Span,
    attrs: Vec<Attr<'core>>,
    mut fn_item: FnItem<'core>,
  ) -> ImplSubitem<'core> {
    if fn_item.method {
      self.core.report(Diag::ImplItemMethod { span });
    }
    if fn_item.generics.inherit {
      self.core.report(Diag::ImplItemInheritGen { span });
    }
    fn_item.generics.inherit = true;
    let def = self.chart_child(parent_def, fn_item.name, vis, false);
    let generics = self.chart_generics(def, parent_generics, fn_item.generics, true);
    let body = self.ensure_implemented(span, fn_item.body);
    let fn_id = self.chart.concrete_fns.push(ConcreteFnDef {
      span,
      def,
      generics,
      method: fn_item.method,
      params: fn_item.params,
      ret_ty: fn_item.ret,
      body,
    });
    self.define_value(span, def, vis, DefValueKind::Fn(FnId::Concrete(fn_id)));
    self.chart_attrs(Some(def), attrs);
    ImplSubitem { span, name: fn_item.name, kind: ImplSubitemKind::Fn(fn_id) }
  }

  fn chart_impl_const(
    &mut self,
    vis: DefId,
    parent_def: DefId,
    parent_generics: GenericsId,
    span: Span,
    attrs: Vec<Attr<'core>>,
    mut const_item: ConstItem<'core>,
  ) -> ImplSubitem<'core> {
    if const_item.generics.inherit {
      self.core.report(Diag::ImplItemInheritGen { span });
    }
    const_item.generics.inherit = true;
    let def = self.chart_child(parent_def, const_item.name, vis, false);
    let generics = self.chart_generics(def, parent_generics, const_item.generics, true);
    let value = self.ensure_implemented(span, const_item.value);
    let const_id = self.chart.concrete_consts.push(ConcreteConstDef {
      span,
      def,
      generics,
      ty: const_item.ty,
      value,
    });
    self.define_value(span, def, vis, DefValueKind::Const(ConstId::Concrete(const_id)));
    self.chart_attrs(Some(def), attrs);
    ImplSubitem { span, name: const_item.name, kind: ImplSubitemKind::Const(const_id) }
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_impl_sig(&mut self, impl_id: ImplId) {
    let impl_def = &self.chart.impls[impl_id];
    self.initialize(impl_def.def, impl_def.generics);
    let ty = self.resolve_trait(&impl_def.trait_);
    let types = take(&mut self.types);
    self.sigs.impls.push_to(impl_id, TypeCtx { types, inner: ImplSig { ty } });
  }

  pub(crate) fn resolve_impl_def(&mut self, impl_id: ImplId) {
    let impl_def = &self.chart.impls[impl_id];
    self.initialize(impl_def.def, impl_def.generics);
    let span = impl_def.span;
    let ty = self.types.import(&self.sigs.impls[impl_id], None).ty;
    let resolved = match &ty {
      ImplType::Trait(trait_id, type_params) => {
        let kind = match &impl_def.kind {
          ImplDefKind::Direct(subitems) => {
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
                self.core.report(Diag::ExtraneousImplItem { span: item.span, name: item.name });
              }
            }
            ResolvedImplKind::Direct { fns, consts }
          }
          ImplDefKind::Indirect(impl_) => {
            ResolvedImplKind::Indirect(self.resolve_impl_type(impl_, &ty))
          }
        };
        Ok(ResolvedImpl { kind, trait_id: *trait_id, become_: Become::Unresolved })
      }
      ImplType::Fn(..) => Err(self.core.report(Diag::CannotImplFn { span })),
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
        self.core.report(diag);
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
    impl_def: &ImplDef<'core>,
  ) -> Result<Option<(ImplId, Vec<usize>)>, Diag<'core>> {
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
    subitems: &[ImplSubitem<'core>],
    type_params: &[Type],
    trait_id: TraitId,
    trait_fn_id: TraitFnId,
  ) -> Result<ConcreteFnId, ErrorGuaranteed> {
    let trait_fn = &self.chart.traits[trait_id].fns[trait_fn_id];
    let name = trait_fn.name;
    let trait_generics = trait_fn.generics;
    let Some(subitem) = subitems.iter().find(|i| i.name == name) else {
      return Err(self.core.report(Diag::IncompleteImpl { span, name }));
    };
    let span = subitem.span;
    let ImplSubitemKind::Fn(fn_id) = subitem.kind else {
      return Err(self.core.report(Diag::WrongImplSubitemKind { span, expected: "fn" }));
    };
    let impl_generics = self.chart.concrete_fns[fn_id].generics;
    let type_params =
      self.resolve_impl_subitem_generics(span, impl_generics, trait_generics, type_params)?;
    let expected =
      self.types.import(&self.sigs.traits[trait_id].fns[trait_fn_id], Some(&type_params));
    let found = self.types.import(&self.sigs.concrete_fns[fn_id], None);
    Self::expect_fn_sig(self.core, self.chart, &mut self.types, span, name, expected, found);
    Ok(fn_id)
  }

  fn resolve_impl_subitem_const(
    &mut self,
    span: Span,
    subitems: &[ImplSubitem<'core>],
    type_params: &[Type],
    trait_id: TraitId,
    trait_const_id: TraitConstId,
  ) -> Result<ConcreteConstId, ErrorGuaranteed> {
    let trait_const = &self.chart.traits[trait_id].consts[trait_const_id];
    let name = trait_const.name;
    let trait_generics = trait_const.generics;
    let Some(subitem) = subitems.iter().find(|i| i.name == name) else {
      return Err(self.core.report(Diag::IncompleteImpl { span, name }));
    };
    let span = subitem.span;
    let ImplSubitemKind::Const(const_id) = subitem.kind else {
      return Err(self.core.report(Diag::WrongImplSubitemKind { span, expected: "const" }));
    };
    let impl_generics = self.chart.concrete_consts[const_id].generics;
    let type_params =
      self.resolve_impl_subitem_generics(span, impl_generics, trait_generics, type_params)?;
    let expected_ty =
      self.types.import(&self.sigs.traits[trait_id].consts[trait_const_id], Some(&type_params)).ty;
    let const_sig = &self.sigs.concrete_consts[const_id];
    let found_ty = self.types.import(const_sig, None).ty;
    if self.types.unify(expected_ty, found_ty).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
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
      Err(self.core.report(Diag::ExpectedImplItemTypeParams { span, expected, found }))?;
    }

    let type_params =
      Vec::from_iter(type_params.iter().copied().chain((impl_params..impl_params + expected).map(
        |i| self.types.new(TypeKind::Param(i, self.sigs.type_params[impl_item_generics].params[i])),
      )));

    let impl_params = self.sigs.impl_params[impl_generics].types.inner.len();
    let impl_item_params = self.sigs.impl_params[impl_item_generics].types.inner.len();
    let trait_params = 1;
    let trait_item_params = self.sigs.impl_params[trait_item_generics].types.inner.len();

    let expected = trait_item_params - trait_params;
    let found = impl_item_params - impl_params;
    if found != expected {
      Err(self.core.report(Diag::ExpectedImplItemImplParams { span, expected, found }))?;
    }

    let trait_item_params =
      self.types.import(&self.sigs.impl_params[trait_item_generics].types, Some(&type_params));
    let impl_item_params =
      self.types.import(&self.sigs.impl_params[impl_item_generics].types, None);
    for (expected, found) in
      trait_item_params[trait_params..].iter().zip(&impl_item_params[impl_params..])
    {
      if self.types.unify_impl_type(expected, found).is_failure() {
        Err(self.core.report(Diag::ExpectedImplItemImplParam {
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
    path: &Path<'core>,
    id: ImplId,
  ) -> (ImplType, TirImpl<'core>) {
    let (type_params, impl_params) =
      self.resolve_generics(path, self.chart.impls[id].generics, true);
    let ty = self.types.import(&self.sigs.impls[id], Some(&type_params)).ty;
    (ty, TirImpl::Def(id, impl_params))
  }
}
