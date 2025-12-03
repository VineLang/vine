use crate::{
  components::{finder::Finder, synthesizer::SyntheticImpl},
  structures::{
    tir::TirImpl,
    types::{Type, TypeCtx, TypeKind, Types},
  },
};

impl Finder<'_> {
  pub(crate) fn find_auto_impls_object(
    &mut self,
    type_params: &[Type],
    types: &Types,
    found: &mut Vec<TypeCtx<TirImpl>>,
  ) {
    // TODO(enricozb): implement the synthetic impl construction to handle
    // various omissions of the four parameters of the Object trait:
    // [Object, Key, Init, Rest].

    if let Some((inv, TypeKind::Object(entries))) = types.kind(type_params[0]) {
      let mut iter = entries.iter();
      if let Some((key, &init)) = iter.next() {
        let init = init.invert_if(inv);
        let rest = iter.map(|(k, &t)| (k.clone(), t.invert_if(inv))).collect();
        let mut types = types.clone();
        let rest = types.new(TypeKind::Object(rest));
        if types.unify(init, type_params[2]).and(types.unify(rest, type_params[3])).is_success() {
          let impl_ = TirImpl::Synthetic(SyntheticImpl::Object(key.clone(), entries.len()));
          found.push(TypeCtx { types, inner: impl_ });
        }
      }
    }
  }
}
