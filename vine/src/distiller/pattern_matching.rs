use std::collections::HashMap;

use vine_util::new_idx;

use crate::{
  ast::{Local, Pat, PatKind},
  resolver::DefId,
};

enum Form {
  Value,
  Space,
  Place,
}

new_idx!(VarId);
new_idx!(ArmId);

struct Row<'t, 'core> {
  cells: HashMap<VarId, &'t Pat<'core>>,
  bindings: Vec<(VarId, &'t Pat<'core>)>,
  arm: ArmId,
}

enum Foo {
  Ref,
  Inverse,
  Tuple,
  Adt(DefId),
}

impl<'core> Pat<'core> {
  fn foo(self: &mut &Self) -> Option<Foo> {
    loop {
      return match &self.kind {
        PatKind::Error(_) => unreachable!(),
        PatKind::Hole | PatKind::Local(_) => None,
        PatKind::Paren(pat) => {
          *self = pat;
          continue;
        }
        PatKind::Adt(generic_path, vec) => todo!(),
        PatKind::Ref(_) => Some(Foo::Ref),
        PatKind::Deref(p) => {
          if let PatKind::Ref(p) = &p.kind {
            *self = p;
            continue;
          }
          None
        }
        PatKind::Inverse(p) => {
          if let PatKind::Inverse(p) = &p.kind {
            *self = p;
            continue;
          }
          #[allow(clippy::explicit_auto_deref)]
          (&**p).foo().map(|_| Foo::Inverse)
        }
        PatKind::Tuple(_) => Some(Foo::Tuple),
      };
    }
  }
}
