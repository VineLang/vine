use vine_util::interner::StringInterner;

use crate::{ast::Ident, checker::Ty};

use super::{NodeType, Resolver};

const PRIMITIVES: [(&str, Ty); 3] = [("u32", Ty::U32), ("f32", Ty::F32), ("IO", Ty::IO)];

impl Resolver {
  pub(super) fn build_prelude(&mut self, interner: &StringInterner<'static>) {
    for (name, ty) in PRIMITIVES {
      self.get_or_insert_child(0, Ident(interner.intern(name))).typ =
        Some(NodeType { generics: Vec::new(), alias: None, ty: Some(ty) })
    }
  }
}
