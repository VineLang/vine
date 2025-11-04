use std::{
  cell::RefCell,
  fmt::{self, Debug},
};

use crate::structures::{ast::Ident, diag::Diag};

pub struct Core {
  pub(crate) diags: RefCell<Vec<Diag>>,
}

impl Core {
  pub fn new() -> &'static Self {
    Box::leak(Box::new(Core { diags: Default::default() }))
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
