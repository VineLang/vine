use std::path::PathBuf;

use vine_util::parser::Parser;

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{VineParser, BRACE},
  },
  structures::{
    ast::{ModItem, ModKind},
    chart::{DefId, GenericsId},
    diag::Diag,
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_mod_item(&mut self) -> Result<ModItem<'core>, Diag<'core>> {
    self.expect(Token::Mod)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let kind = if self.eat(Token::Eq)? {
      let span = self.start_span();
      let path = self.parse_string()?;
      let span = self.end_span(span);
      self.expect(Token::Semi)?;
      ModKind::Unloaded(span, PathBuf::from(path))
    } else {
      let span = self.start_span();
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      let span = self.end_span(span);
      ModKind::Loaded(span, items)
    };
    Ok(ModItem { name, generics, kind })
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_mod_item(&self, m: &ModItem<'core>) -> Doc<'src> {
    Doc::concat([
      Doc("mod "),
      Doc(m.name),
      self.fmt_generic_params(&m.generics),
      match &m.kind {
        ModKind::Loaded(span, items) => Doc::concat([
          Doc(" "),
          self.fmt_block_like(*span, items.iter().map(|x| (x.span, self.fmt_item(x)))),
        ]),
        ModKind::Unloaded(span, _) => Doc::concat([Doc(" = "), self.fmt_verbatim(*span), Doc(";")]),
        ModKind::Error(_) => unreachable!(),
      },
    ])
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_mod(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    vis: DefId,
    member_vis: DefId,
    mod_item: ModItem<'core>,
  ) -> DefId {
    let def = self.chart_child(parent, mod_item.name, member_vis, true);
    let generics = self.chart_generics(def, parent_generics, mod_item.generics, true);
    self.chart_mod_kind(vis, mod_item.kind, def, generics);
    def
  }
}
