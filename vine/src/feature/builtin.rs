use super::*;

#[derive_(Debug, Clone, Visit!)]
pub struct AttrBuiltin {
  pub builtin: Ident,
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
