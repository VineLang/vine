use crate::{
  components::{finder::Finder, synthesizer::SyntheticImpl},
  structures::{
    tir::TirImpl,
    types::{Type, TypeCtx, TypeKind, Types},
  },
};

impl Finder<'_> {
  pub(crate) fn find_auto_impls_tuple(
    &mut self,
    type_params: &[Type],
    types: &Types,
    found: &mut Vec<TypeCtx<TirImpl>>,
  ) {
    if let Some((inv, TypeKind::Tuple(elements))) = types.kind(type_params[0]) {
      if let [init, ref rest @ ..] = **elements {
        let init = init.invert_if(inv);
        let rest = rest.iter().map(|t| t.invert_if(inv)).collect();
        let mut types = types.clone();
        let rest = types.new(TypeKind::Tuple(rest));
        if types.unify(init, type_params[1]).and(types.unify(rest, type_params[2])).is_success() {
          let impl_ = TirImpl::Synthetic(SyntheticImpl::Tuple(elements.len()));
          found.push(TypeCtx { types, inner: impl_ });
        }
      }
    } else if let Some((inv, TypeKind::Tuple(rest))) = types.kind(type_params[2]) {
      let len = 1 + rest.len();
      let init = type_params[1];
      let rest = rest.iter().map(|t| t.invert_if(inv));
      let mut types = types.clone();
      let tuple = types.new(TypeKind::Tuple([init].into_iter().chain(rest).collect()));
      if types.unify(tuple, type_params[0]).is_success() {
        let impl_ = TirImpl::Synthetic(SyntheticImpl::Tuple(len));
        found.push(TypeCtx { types, inner: impl_ });
      }
    }
  }
}
