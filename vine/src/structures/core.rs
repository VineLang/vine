use std::{
  cell::RefCell,
  fmt::{self, Debug},
};

use vine_util::{arena::BytesArena, idx::IdxVec, new_idx};

use crate::structures::{
  ast::Ident,
  diag::{Diag, FileInfo},
};

#[derive(Default)]
pub struct CoreArenas {
  bytes: BytesArena,
}

new_idx!(pub FileId);

pub struct Core {
  arenas: &'static CoreArenas,
  pub(crate) diags: RefCell<Vec<Diag>>,
  pub(crate) files: RefCell<IdxVec<FileId, FileInfo>>,
  pub debug: bool,
}

impl Core {
  pub fn new(debug: bool) -> &'static Self {
    Box::leak(Box::new(Core {
      arenas: Box::leak(Box::new(CoreArenas::default())),
      diags: Default::default(),
      files: Default::default(),
      debug,
    }))
  }

  pub fn ident(&self, str: &str) -> Ident {
    Ident(str.to_owned())
  }

  pub fn alloc_str(&self, str: &str) -> &str {
    self.arenas.bytes.alloc_str(str)
  }
}

impl Debug for Core {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Core {{ .. }}")
  }
}
