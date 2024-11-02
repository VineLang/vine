use vine_util::interner::StringInterner;

use crate::{ast::Ident, checker::Type};

use super::{NodeType, Resolver};

const PRIMITIVES: [(&str, Type); 3] = [("u32", Type::U32), ("f32", Type::F32), ("IO", Type::IO)];

impl Resolver {
  pub(super) fn build_prelude(&mut self, interner: &StringInterner<'static>) {
    for (name, ty) in PRIMITIVES {
      self.get_or_insert_child(0, Ident(interner.intern(name))).typ =
        Some(NodeType { generics: Vec::new(), alias: None, ty: Some(ty) })
    }
  }
}
