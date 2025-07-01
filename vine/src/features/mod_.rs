use std::path::PathBuf;

use vine_util::parser::Parser;

use crate::{
  components::{
    lexer::Token,
    parser::{VineParser, BRACE},
  },
  structures::{
    ast::{ModItem, ModKind},
    diag::Diag,
  },
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_mod_item(&mut self) -> Result<ModItem<'core>, Diag<'core>> {
    self.expect(Token::Mod)?;
    let name = self.parse_ident()?;
    if self.eat(Token::Eq)? {
      let span = self.start_span();
      let path = self.parse_string()?;
      let span = self.end_span(span);
      self.expect(Token::Semi)?;
      Ok(ModItem { name, kind: ModKind::Unloaded(span, PathBuf::from(path)) })
    } else {
      let span = self.start_span();
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      let span = self.end_span(span);
      Ok(ModItem { name, kind: ModKind::Loaded(span, items) })
    }
  }
}
