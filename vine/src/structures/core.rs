use std::{
  cell::RefCell,
  fmt::{self, Debug},
};

use vine_util::{idx::IdxVec, new_idx};

use crate::structures::{
  ast::Ident,
  diag::{Diag, FileInfo},
};

new_idx!(pub FileId);

pub struct Core {
  pub(crate) diags: RefCell<Vec<Diag>>,
  pub(crate) files: RefCell<IdxVec<FileId, FileInfo>>,
  pub debug: bool,
}

impl Core {
  pub fn new(debug: bool) -> &'static Self {
    Box::leak(Box::new(Core { diags: Default::default(), files: Default::default(), debug }))
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
