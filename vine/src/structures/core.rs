use std::fmt::{self, Debug};

use crate::structures::ast::Ident;

pub struct Core {}

impl Core {
  pub fn new() -> &'static Self {
    Box::leak(Box::new(Core {}))
  }

  pub fn ident(&self, str: &str) -> Ident {
    Ident(str.to_owned())
  }
}

impl Debug for Core {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Core {{ .. }}")
  }
}
