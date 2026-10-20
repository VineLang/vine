use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrAllow {
  pub lints: Children<Ident>,
}

impl ParseAttr<AttrAllow> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrAllow>, Diag> {
    if name == "allow" {
      let lints =
        self.parse_children(Some(Token::OpenParen), Some(Token::Comma), Some(Token::CloseParen))?;
      Ok(Some(AttrAllow { lints }))
    } else {
      Ok(None)
    }
  }
}

#[derive_(Debug, Clone, Copy)]
pub enum Lint {}
