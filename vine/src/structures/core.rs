use std::{
  cell::RefCell,
  fmt::{self, Debug},
};

use vine_util::{arena::BytesArena, idx::IdxVec, interner::StringInterner, new_idx};

use crate::structures::{
  ast::Ident,
  diag::{Diag, FileInfo},
};

#[derive(Default)]
pub struct CoreArenas {
  bytes: BytesArena,
}

new_idx!(pub FileId);

pub struct Core<'core> {
  arenas: &'core CoreArenas,
  interner: StringInterner<'core>,
  pub(crate) diags: RefCell<Vec<Diag<'core>>>,
  pub(crate) files: RefCell<IdxVec<FileId, FileInfo>>,
  pub debug: bool,
}

impl<'core> Core<'core> {
  pub fn new(arenas: &'core CoreArenas, debug: bool) -> Self {
    Core {
      arenas,
      interner: StringInterner::new(&arenas.bytes),
      diags: Default::default(),
      files: Default::default(),
      debug,
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
