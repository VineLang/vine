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
    ast::{Attr, ConstItem, FnItem, GenericParams, ItemKind, Span, TraitItem, Vis},
    chart::{
      ConstId, DefId, DefTraitKind, DefValueKind, FnId, GenericsDef, GenericsId, TraitConst,
      TraitConstId, TraitDef, TraitFn, TraitFnId, TraitId,
    },
    diag::Diag,
    signatures::{ConstSig, FnSig, TraitSig},
    types::TypeCtx,
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_trait_item(&mut self) -> Result<TraitItem<'core>, Diag<'core>> {
    self.expect(Token::Trait)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let items = self.parse_delimited(BRACE, Self::parse_item)?;
    Ok(TraitItem { name, generics, items })
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_trait_item(&self, span: Span, t: &TraitItem<'core>) -> Doc<'src> {
    Doc::concat([
      Doc("trait "),
      Doc(t.name),
      self.fmt_generic_params(&t.generics),
      Doc(" "),
      self.fmt_block_like(span, t.items.iter().map(|i| (i.span, self.fmt_item(i)))),
    ])
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_trait(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: DefId,
    member_vis: DefId,
    trait_item: TraitItem<'core>,
  ) -> DefId {
    let def = self.chart_child(parent, trait_item.name, member_vis, true);
    let generics = self.chart_generics(def, parent_generics, trait_item.generics, false);
    let trait_id = self.chart.traits.next_index();
    let mut consts = IdxVec::new();
    let mut fns = IdxVec::new();
    for subitem in trait_item.items {
      let span = subitem.span;
      let attrs = subitem.attrs;
      if !matches!(subitem.vis, Vis::Private) {
        self.core.report(Diag::TraitItemVis { span });
      }
      match subitem.kind {
        ItemKind::Fn(item) => {
          self.chart_trait_fn(vis, def, trait_id, generics, &mut fns, span, attrs, item);
        }
        ItemKind::Const(item) => {
          self.chart_trait_const(vis, def, trait_id, generics, &mut consts, span, attrs, item);
        }
        _ => {
          self.core.report(Diag::InvalidTraitItem { span });
        }
      }
    }
    let trait_ = TraitDef { span, def, name: trait_item.name, generics, consts, fns };
    self.chart.traits.push_to(trait_id, trait_);
    self.define_trait(span, def, vis, DefTraitKind::Trait(trait_id));
    def
  }

  fn chart_trait_fn(
    &mut self,
    vis: DefId,
    def: DefId,
    trait_id: TraitId,
    trait_generics: GenericsId,
    fns: &mut IdxVec<TraitFnId, TraitFn<'core>>,
    span: Span,
    attrs: Vec<Attr>,
    fn_item: FnItem<'core>,
  ) {
    if fn_item.body.is_some() {
      self.core.report(Diag::ImplementedTraitItem { span });
    }
    let generics =
      self.chart_trait_subitem_generics(span, def, trait_id, trait_generics, fn_item.generics);
    let trait_fn_id = fns.push(TraitFn {
      method: fn_item.method,
      name: fn_item.name,
      generics,
      params: fn_item.params,
      ret_ty: fn_item.ret,
    });
    let def = self.chart_child(def, fn_item.name, vis, true);
    let kind = DefValueKind::Fn(FnId::Abstract(trait_id, trait_fn_id));
    self.define_value(span, def, vis, kind);
    self.chart_attrs(Some(def), attrs);
  }

  fn chart_trait_const(
    &mut self,
    vis: DefId,
    def: DefId,
    trait_id: TraitId,
    trait_generics: GenericsId,
    consts: &mut IdxVec<TraitConstId, TraitConst<'core>>,
    span: Span,
    attrs: Vec<Attr>,
    const_item: ConstItem<'core>,
  ) {
    if const_item.value.is_some() {
      self.core.report(Diag::ImplementedTraitItem { span });
    }
    let generics =
      self.chart_trait_subitem_generics(span, def, trait_id, trait_generics, const_item.generics);
    let trait_const_id =
      consts.push(TraitConst { name: const_item.name, generics, ty: const_item.ty });
    let def = self.chart_child(def, const_item.name, vis, true);
    let kind = DefValueKind::Const(ConstId::Abstract(trait_id, trait_const_id));
    self.define_value(span, def, vis, kind);
    self.chart_attrs(Some(def), attrs);
  }

  fn chart_trait_subitem_generics(
    &mut self,
    span: Span,
    def: DefId,
    trait_id: TraitId,
    trait_generics: GenericsId,
    generics: GenericParams<'core>,
  ) -> GenericsId {
    if generics.inherit {
      self.core.report(Diag::TraitItemInheritGen { span });
    }
    self.chart.generics.push(GenericsDef {
      span,
      def,
      parent: Some(trait_generics),
      type_params: generics.types,
      impl_params: generics.impls,
      impl_allowed: true,
      trait_: Some(trait_id),
    })
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_trait_sig(&mut self, trait_id: TraitId) {
    let trait_def = &self.chart.traits[trait_id];
    let sig = TraitSig {
      consts: IdxVec::from_iter(trait_def.consts.values().map(|trait_const| {
        self.initialize(trait_def.def, trait_const.generics);
        let ty = self.resolve_ty(&trait_const.ty, false);
        TypeCtx { types: take(&mut self.types), inner: ConstSig { ty } }
      })),
      fns: IdxVec::from_iter(trait_def.fns.values().map(|trait_fn| {
        self.initialize(trait_def.def, trait_fn.generics);
        let (params, ret_ty) = self._resolve_fn_sig(&trait_fn.params, &trait_fn.ret_ty);
        TypeCtx { types: take(&mut self.types), inner: FnSig { params, ret_ty } }
      })),
    };
    self.sigs.traits.push_to(trait_id, sig);
  }
}
