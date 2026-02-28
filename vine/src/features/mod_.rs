use vine_util::parser::Parse;

use crate::{
  components::{
    charter::{ChartedItem, Charter},
    lexer::Token,
    parser::{BRACE, Parser},
  },
  structures::{
    ast::{ItemKind, ModItem, ModKind, Span},
    chart::{DefId, GenericsId, VisId},
    diag::Diag,
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_mod_item(&mut self) -> Result<(Span, ItemKind), Diag> {
    let mod_span = self.span();
    self.expect(Token::Mod)?;
    if self.eat(Token::Semi)? {
      return Ok((mod_span, ItemKind::OuterMod));
    }
    let name_span = self.span();
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let kind = if self.check(Token::OpenBrace) {
      let span = self.start_span();
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      let span = self.end_span(span);
      ModKind::Loaded(span, None, items)
    } else {
      self.eat(Token::Semi)?;
      ModKind::Unloaded
    };
    Ok((name_span, ItemKind::Mod(ModItem { name, generics, kind })))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_mod_item(&self, m: &ModItem) -> Doc<'src> {
    Doc::concat([
      Doc("mod "),
      Doc(m.name.clone()),
      self.fmt_generic_params(&m.generics),
      match &m.kind {
        ModKind::Loaded(span, _, items) => Doc::concat([
          Doc(" "),
          self.fmt_block_like(*span, items.iter().map(|x| (x.span, self.fmt_item(x)))),
        ]),
        ModKind::Unloaded => Doc::concat([Doc(";")]),
        ModKind::Error(_) => unreachable!(),
      },
    ])
  }
}

impl Charter<'_> {
  pub(crate) fn chart_mod(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: VisId,
    member_vis: VisId,
    mod_item: ModItem,
  ) -> ChartedItem {
    let def = self.chart_child(parent, span, mod_item.name, member_vis, true);
    // Add an artificial reference to submodules to prevent erroneous unused item
    // warnings.
    self.annotations.record_reference(span, span);
    let generics = self.chart_generics(def, parent_generics, mod_item.generics, true);
    match mod_item.kind {
      ModKind::Loaded(inner_span, _, _) => {
        self.annotations.definitions.entry(span).or_default().insert(inner_span);
        self.chart_mod_kind(vis, mod_item.kind, def, generics);
        ChartedItem::Mod(def)
      }
      ModKind::Error(err) => ChartedItem::Error(err),
      ModKind::Unloaded => unreachable!(),
    }
  }
}
