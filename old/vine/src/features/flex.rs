use crate::{
  components::finder::Finder,
  structures::{
    tir::TirImpl,
    types::{Inverted, Type, TypeCtx, TypeKind, Types},
  },
};

impl Finder<'_> {
  pub(crate) fn find_auto_impls_fork(
    &mut self,
    type_params: &[Type],
    types: &Types,
    found: &mut Vec<TypeCtx<TirImpl>>,
  ) {
    match types.kind(type_params[0]) {
      Some((Inverted(false), TypeKind::Fn(_))) | Some((_, TypeKind::Default)) => {
        if let Some(dup) = self.chart.builtins.duplicate {
          found.push(TypeCtx { types: types.clone(), inner: TirImpl::Def(dup, vec![]) });
        }
      }
      Some((Inverted(false), TypeKind::Closure(id, flex, ..))) if flex.fork() => {
        found.push(TypeCtx { types: types.clone(), inner: TirImpl::ForkClosure(*id) });
      }
      _ => {}
    }
  }

  pub(crate) fn find_auto_impls_drop(
    &mut self,
    type_params: &[Type],
    types: &Types,
    found: &mut Vec<TypeCtx<TirImpl>>,
  ) {
    match types.kind(type_params[0]) {
      Some((Inverted(false), TypeKind::Fn(_))) | Some((_, TypeKind::Default)) => {
        if let Some(erase) = self.chart.builtins.erase {
          found.push(TypeCtx { types: types.clone(), inner: TirImpl::Def(erase, vec![]) });
        }
      }
      Some((Inverted(false), TypeKind::Closure(id, flex, ..))) if flex.drop() => {
        found.push(TypeCtx { types: types.clone(), inner: TirImpl::DropClosure(*id) });
      }
      _ => {}
    }
  }
}
