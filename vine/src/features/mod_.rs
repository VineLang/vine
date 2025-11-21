use vine_util::parser::Parser;

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{BRACE, VineParser},
  },
  structures::{
    ast::{ModItem, ModKind},
    chart::{DefId, GenericsId, VisId},
    diag::Diag,
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl VineParser<'_> {
  pub(crate) fn parse_mod_item(&mut self) -> Result<ModItem, Diag> {
    self.expect(Token::Mod)?;
    let name_span = self.span();
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let kind = if self.check(Token::OpenBrace) {
      let span = self.start_span();
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      let span = self.end_span(span);
      ModKind::Loaded(span, items)
    } else if self.eat(Token::Eq)? {
      let path_span = self.start_span();
      let path = self.parse_string()?;
      let path_span = self.end_span(path_span);
      self.expect(Token::Semi)?;
      ModKind::Unloaded(path_span, Some(path))
    } else {
      self.expect(Token::Semi)?;
      ModKind::Unloaded(name_span, None)
    };
    Ok(ModItem { name, generics, kind })
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_mod_item(&self, m: &ModItem) -> Doc<'src> {
    Doc::concat([
      Doc("mod "),
      Doc(m.name.clone()),
      self.fmt_generic_params(&m.generics),
      match &m.kind {
        ModKind::Loaded(span, items) => Doc::concat([
          Doc(" "),
          self.fmt_block_like(*span, items.iter().map(|x| (x.span, self.fmt_item(x)))),
        ]),
        ModKind::Unloaded(span, Some(_)) => {
          Doc::concat([Doc(" = "), self.fmt_verbatim(*span), Doc(";")])
        }
        ModKind::Unloaded(_, None) => Doc::concat([Doc(";")]),
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
    vis: VisId,
    member_vis: VisId,
    mod_item: ModItem,
  ) -> DefId {
    let def = self.chart_child(parent, mod_item.name, member_vis, true);
    let generics = self.chart_generics(def, parent_generics, mod_item.generics, true);
    self.chart_mod_kind(vis, mod_item.kind, def, generics);
    def
  }
}
