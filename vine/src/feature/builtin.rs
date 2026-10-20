use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrBuiltin {
  pub builtin: Spanned<Ident>,
}

impl ParseAttr<AttrBuiltin> for Parser<'_> {
  fn _parse_attr(&mut self, name: &str) -> Result<Option<AttrBuiltin>, Diag> {
    if name == "builtin" {
      let builtin = self.parse()?;
      Ok(Some(AttrBuiltin { builtin }))
    } else {
      Ok(None)
    }
  }
}

#[derive_(Debug, Clone, for_field!)]
pub struct Builtins {
  foo: Option<u32>,
}

impl Builtins {
  fn set(&mut self, ident: Ident, value: u64) {
    for_field!(builtin in Builtins {
      match &*ident.0 {
        $((stringify!($builtin)) => self.$builtin = value.try_into().ok(),)*
        _ => todo!(),
      }
    })
  }
}
