use crate::{
  components::{finder::Finder, synthesizer::SyntheticImpl},
  structures::{
    tir::TirImpl,
    types::{Inverted, Type, TypeCtx, TypeKind, Types},
  },
};

impl Finder<'_> {
  pub(crate) fn find_auto_impls_object(
    &mut self,
    type_params: &[Type],
    types: &Types,
    found: &mut Vec<TypeCtx<TirImpl>>,
  ) {
    let [object_ty, key_ty, init_ty, rest_ty] = *type_params else { unreachable!() };

    match types.kind(object_ty) {
      Some((inv, TypeKind::Object(entries))) => {
        let mut iter = entries.iter();
        if let Some((key, &init)) = iter.next() {
          let init = init.invert_if(inv);
          let rest = iter.map(|(k, &t)| (k.clone(), t.invert_if(inv))).collect();
          let mut types = types.clone();
          let rest = types.new(TypeKind::Object(rest));
          if types.unify(init, init_ty).and(types.unify(rest, rest_ty)).is_success() {
            let impl_ = TirImpl::Synthetic(SyntheticImpl::Object(key.clone(), entries.len()));
            found.push(TypeCtx { types, inner: impl_ });
          }
        }
      }
      None => {
        if let Some((Inverted(false) /* ??? */, TypeKind::Key(key))) = types.kind(key_ty)
          && let Some((inv, TypeKind::Object(rest_entries))) = types.kind(rest_ty)
          && !rest_entries.contains_key(key)
        {
          let object_entries = rest_entries
            .iter()
            .map(|(k, &t)| (k.clone(), t.invert_if(inv)))
            .chain(vec![(key.clone(), init_ty)])
            .collect();
          let mut types = types.clone();
          let object = types.new(TypeKind::Object(object_entries));
          if types.unify(object, object_ty).is_success() {
            let impl_ =
              TirImpl::Synthetic(SyntheticImpl::Object(key.clone(), rest_entries.len() + 1));
            found.push(TypeCtx { types, inner: impl_ });
          }
        }
      }
      _ => {}
    }
  }
}
