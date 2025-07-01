use vine_util::{idx::IdxVec, parser::Parser};

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{VineParser, BRACE},
  },
  structures::{
    ast::{
      Attr, ConstItem, FnItem, Generics, Ident, ImplParam, ItemKind, Path, Span, Trait, TraitItem,
      TraitKind, Ty, TyKind, Vis,
    },
    chart::{
      ConstId, DefId, DefTraitKind, DefValueKind, FnId, GenericsDef, GenericsId, TraitConst,
      TraitConstId, TraitDef, TraitFn, TraitFnId, TraitId,
    },
    diag::Diag,
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
    span: Span,
    vis: DefId,
    member_vis: DefId,
    trait_item: TraitItem<'core>,
  ) -> DefId {
    let def = self.chart_child(parent, trait_item.name, member_vis, true);
    let generics = self.chart_generics(def, trait_item.generics, false);
    let subitem_generics = self.chart_trait_subitem_generics(span, trait_item.name, generics);
    let trait_id = self.chart.traits.next_index();
    let mut consts = IdxVec::new();
    let mut fns = IdxVec::new();
    for subitem in trait_item.items {
      let span = subitem.span;
      if !matches!(subitem.vis, Vis::Private) {
        self.core.report(Diag::TraitItemVis { span });
      }
      match subitem.kind {
        ItemKind::Fn(fn_item) => {
          self.chart_trait_fn(vis, def, trait_id, &mut fns, span, subitem.attrs, fn_item);
        }
        ItemKind::Const(const_item) => {
          self.chart_trait_const(vis, def, trait_id, &mut consts, span, subitem.attrs, const_item);
        }
        _ => {
          self.core.report(Diag::InvalidTraitItem { span });
        }
      }
    }
    let trait_ =
      TraitDef { span, def, name: trait_item.name, generics, subitem_generics, consts, fns };
    self.chart.traits.push_to(trait_id, trait_);
    self.define_trait(span, def, vis, DefTraitKind::Trait(trait_id));
    def
  }

  fn chart_trait_fn(
    &mut self,
    vis: DefId,
    def: DefId,
    trait_id: TraitId,
    fns: &mut IdxVec<TraitFnId, TraitFn<'core>>,
    span: Span,
    attrs: Vec<Attr>,
    fn_item: FnItem<'core>,
  ) {
    if !fn_item.generics.impls.is_empty() || !fn_item.generics.types.is_empty() {
      self.core.report(Diag::TraitItemGen { span });
    }
    if fn_item.body.is_some() {
      self.core.report(Diag::ImplementedTraitItem { span });
    }
    let trait_fn_id = fns.push(TraitFn {
      name: fn_item.name,
      method: fn_item.method,
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
    consts: &mut IdxVec<TraitConstId, TraitConst<'core>>,
    span: Span,
    attrs: Vec<Attr>,
    const_item: ConstItem<'core>,
  ) {
    if !const_item.generics.impls.is_empty() || !const_item.generics.types.is_empty() {
      self.core.report(Diag::TraitItemGen { span });
    }
    if const_item.value.is_some() {
      self.core.report(Diag::ImplementedTraitItem { span });
    }
    let trait_const_id = consts.push(TraitConst { name: const_item.name, ty: const_item.ty });
    let def = self.chart_child(def, const_item.name, vis, true);
    let kind = DefValueKind::Const(ConstId::Abstract(trait_id, trait_const_id));
    self.define_value(span, def, vis, kind);
    self.chart_attrs(Some(def), attrs);
  }

  fn chart_trait_subitem_generics(
    &mut self,
    span: Span,
    trait_name: Ident<'core>,
    generics: GenericsId,
  ) -> GenericsId {
    let generics = self.chart.generics[generics].clone();
    self.chart.generics.push(GenericsDef {
      impl_params: vec![ImplParam {
        span,
        name: None,
        trait_: Trait {
          span,
          kind: Box::new(TraitKind::Path(Path {
            span,
            absolute: false,
            segments: vec![trait_name],
            generics: Some(Generics {
              span,
              types: generics
                .type_params
                .iter()
                .map(|param| Ty {
                  span,
                  kind: Box::new(TyKind::Path(Path {
                    span,
                    absolute: false,
                    segments: vec![param.name],
                    generics: None,
                  })),
                })
                .collect(),
              impls: vec![],
            }),
          })),
        },
      }],
      ..generics
    })
  }
}
