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
    ast::{Attr, ConstItem, Flex, FnItem, GenericParams, ItemKind, Span, TraitItem, Vis},
    chart::{
      ConstId, DefId, DefTraitKind, DefValueKind, FnId, GenericsDef, GenericsId, TraitConst,
      TraitConstId, TraitDef, TraitFn, TraitFnId, TraitId, VisId,
    },
    diag::Diag,
    signatures::{ConstSig, FnSig, TraitSig},
    types::TypeCtx,
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_trait_item(&mut self) -> Result<(Span, ItemKind), Diag> {
    self.expect(Token::Trait)?;
    let name_span = self.span();
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let items_span = self.start_span();
    let items = self.parse_delimited(BRACE, Self::parse_item)?;
    let items_span = self.end_span(items_span);
    Ok((name_span, ItemKind::Trait(TraitItem { name, generics, items_span, items })))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_trait_item(&self, t: &TraitItem) -> Doc<'src> {
    Doc::concat([
      Doc("trait "),
      Doc(t.name.clone()),
      self.fmt_generic_params(&t.generics),
      Doc(" "),
      self.fmt_block_like(t.items_span, t.items.iter().map(|i| (i.span, self.fmt_item(i)))),
    ])
  }
}

impl Charter<'_> {
  pub(crate) fn chart_trait(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: VisId,
    member_vis: VisId,
    trait_item: TraitItem,
  ) -> ChartedItem {
    let def = self.chart_child(parent, span, trait_item.name.clone(), member_vis, true);
    let generics = self.chart_generics(def, parent_generics, trait_item.generics, false);
    let trait_id = self.chart.traits.next_index();
    let mut consts = IdxVec::new();
    let mut fns = IdxVec::new();
    for subitem in trait_item.items {
      let span = subitem.name_span;
      let attrs = subitem.attrs;
      let unsafe_ = subitem.unsafe_;
      if !matches!(subitem.vis, Vis::Private) {
        self.diags.error(Diag::TraitItemVis { span });
      }
      match subitem.kind {
        ItemKind::Fn(item) => {
          self.chart_trait_fn(vis, def, trait_id, generics, &mut fns, span, attrs, unsafe_, item);
        }
        ItemKind::Const(item) => {
          self.chart_trait_const(
            vis,
            def,
            trait_id,
            generics,
            &mut consts,
            span,
            attrs,
            unsafe_,
            item,
          );
        }
        _ => {
          self.diags.error(Diag::InvalidTraitItem { span });
        }
      }
    }
    let trait_ =
      TraitDef { span, def, name: trait_item.name, generics, consts, fns, unsafe_: false };
    self.chart.traits.push_to(trait_id, trait_);
    self.define_trait(span, def, vis, DefTraitKind::Trait(trait_id));
    ChartedItem::Trait(def, trait_id)
  }

  fn chart_trait_fn(
    &mut self,
    vis: VisId,
    def: DefId,
    trait_id: TraitId,
    trait_generics: GenericsId,
    fns: &mut IdxVec<TraitFnId, TraitFn>,
    span: Span,
    attrs: Vec<Attr>,
    unsafe_: bool,
    fn_item: FnItem,
  ) {
    if fn_item.body.is_some() {
      self.diags.error(Diag::ImplementedTraitItem { span });
    }
    let generics =
      self.chart_trait_subitem_generics(span, def, trait_id, trait_generics, fn_item.generics);
    let trait_fn_id = fns.push(TraitFn {
      span,
      method: fn_item.method,
      name: fn_item.name.clone(),
      generics,
      params: fn_item.params,
      ret_ty: fn_item.ret,
      unsafe_,
    });
    let def = self.chart_child(def, span, fn_item.name, vis, true);
    let fn_id = FnId::Abstract(trait_id, trait_fn_id);
    self.define_value(span, def, vis, DefValueKind::Fn(fn_id));
    let charted_item = ChartedItem::Fn(def, fn_id);
    self.chart_attrs(charted_item, attrs);
  }

  fn chart_trait_const(
    &mut self,
    vis: VisId,
    def: DefId,
    trait_id: TraitId,
    trait_generics: GenericsId,
    consts: &mut IdxVec<TraitConstId, TraitConst>,
    span: Span,
    attrs: Vec<Attr>,
    unsafe_: bool,
    const_item: ConstItem,
  ) {
    if const_item.value.is_some() {
      self.diags.error(Diag::ImplementedTraitItem { span });
    }
    let generics =
      self.chart_trait_subitem_generics(span, def, trait_id, trait_generics, const_item.generics);
    let trait_const_id = consts.push(TraitConst {
      span,
      name: const_item.name.clone(),
      generics,
      ty: const_item.ty,
      unsafe_,
    });
    let def = self.chart_child(def, span, const_item.name, vis, true);
    let const_id = ConstId::Abstract(trait_id, trait_const_id);
    self.define_value(span, def, vis, DefValueKind::Const(const_id));
    let charted_item = ChartedItem::Const(def, const_id);
    self.chart_attrs(charted_item, attrs);
  }

  fn chart_trait_subitem_generics(
    &mut self,
    span: Span,
    def: DefId,
    trait_id: TraitId,
    trait_generics: GenericsId,
    generics: GenericParams,
  ) -> GenericsId {
    if generics.inherit {
      self.diags.error(Diag::TraitItemInheritGen { span });
    }
    self.chart.generics.push(GenericsDef {
      span,
      def,
      parent: Some(trait_generics),
      type_params: generics.types,
      impl_params: generics.impls,
      impl_allowed: true,
      global_flex: Flex::None,
      trait_: Some(trait_id),
    })
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_trait_sig(&mut self, trait_id: TraitId) {
    let trait_def = &self.chart.traits[trait_id];
    let sig = TraitSig {
      consts: IdxVec::from_iter(trait_def.consts.values().map(|trait_const| {
        self.initialize(trait_def.def, trait_const.generics, false);
        let ty = self.resolve_ty(&trait_const.ty, false);

        let hover = format!(
          "const {}::{}{}: {};",
          trait_def.name,
          trait_const.name,
          self.show_generics(self.cur_generics, true),
          self.types.show(self.chart, ty),
        );
        self.annotations.record_signature(trait_const.span, hover);

        TypeCtx { types: take(&mut self.types), inner: ConstSig { ty } }
      })),
      fns: IdxVec::from_iter(trait_def.fns.values().map(|trait_fn| {
        self.initialize(trait_def.def, trait_fn.generics, false);
        let names = trait_fn.params.iter().map(|x| self.param_name(x)).collect();
        let (params, ret_ty) = self._resolve_fn_sig(&trait_fn.params, &trait_fn.ret_ty);
        let sig = FnSig { names, param_tys: params, ret_ty };

        let hover = format!(
          "fn {}::{}{}{};",
          trait_def.name,
          trait_fn.name,
          self.show_generics(self.cur_generics, true),
          self.types.show_fn_sig(self.chart, &sig),
        );
        self.annotations.record_signature(trait_fn.span, hover);

        TypeCtx { types: take(&mut self.types), inner: sig }
      })),
    };
    self.sigs.traits.push_to(trait_id, sig);

    let hover =
      format!("trait {}{};", trait_def.name, self.show_generics(trait_def.generics, true));
    self.annotations.record_signature(trait_def.span, hover);
  }
}
