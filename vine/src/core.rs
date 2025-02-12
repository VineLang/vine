use std::{
  cell::RefCell,
  fmt::{self, Debug},
};

use vine_util::{arena::BytesArena, interner::StringInterner};

use crate::{
  ast::Ident,
  diag::{Diag, FileInfo},
};

#[derive(Default)]
pub struct CoreArenas {
  bytes: BytesArena,
}

pub struct Core<'core> {
  arenas: &'core CoreArenas,
  interner: StringInterner<'core>,
  pub(crate) diags: RefCell<Vec<Diag<'core>>>,
  pub(crate) files: RefCell<Vec<FileInfo>>,
}

impl<'core> Core<'core> {
  pub fn new(arenas: &'core CoreArenas) -> Self {
    Core {
      arenas,
      interner: StringInterner::new(&arenas.bytes),
      diags: Default::default(),
      files: Default::default(),
    }
  }

  pub fn ident(&self, str: &str) -> Ident<'core> {
    Ident(self.interner.intern(str))
  }

  pub fn alloc_str(&self, str: &str) -> &'core str {
    self.arenas.bytes.alloc_str(str)
  }
}

impl<'core> Debug for Core<'core> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Core {{ .. }}")
  }
}
