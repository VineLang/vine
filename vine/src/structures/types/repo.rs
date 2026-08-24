use std::{
  borrow::Cow,
  collections::{BTreeMap, BTreeSet},
};

use vine_util::{
  idx::{Counter, IdxSlab},
  new_idx,
  stack_stack::StackStack,
};

use crate::structures::types::{Inverted, Type, TypeIdx, TypeKind, UnifyResult};

#[derive(Default, Debug)]
pub struct TypesRepo {
  types: Counter<TypeIdx>,
  kinds: IdxSlab<TypeKindId, TypeKind>,
  _dead_kinds: Vec<TypeKindId>,
}

impl TypesRepo {
  pub fn abandon(&mut self, patch: TypesPatch) {
    for (_, node) in patch.nodes {
      if let TypeNode::Root(TypeState::Known(_, kind)) = node {
        self.kinds.remove(kind);
      }
    }
  }

  pub fn kind(&self, kind: TypeKindId) -> &TypeKind {
    &self.kinds[kind]
  }
}

#[derive(Default, Debug)]
pub struct TypesPatch {
  nodes: BTreeMap<TypeIdx, TypeNode>,
  new: Vec<TypeIdx>,
}

impl TypesPatch {
  pub fn is_empty(&self) -> bool {
    self.nodes.is_empty()
  }
}

new_idx!(pub TypeKindId);

#[derive(Debug, Clone, Copy)]
enum TypeNode {
  Root(TypeState),
  Child(Type),
}

#[derive(Debug, Clone, Copy)]
pub enum TypeState {
  Known(Inverted, TypeKindId),
  Unknown { self_inv: bool },
}

impl TypeState {
  pub fn is_known(self) -> bool {
    matches!(self, TypeState::Known(..))
  }

  pub fn invert_if(self, inv: Inverted) -> Self {
    match self {
      TypeState::Known(inv_, kind) => TypeState::Known(inv ^ inv_, kind),
      _ => self,
    }
  }
}

#[derive(Debug)]
pub struct TypesCommit<'a, R = TypesRepo> {
  patch: &'a mut TypesPatch,
  parents: StackStack<'a, TypesPatch>,
  pub repo: &'a mut R,
}

impl<'a, R> TypesCommit<'a, R> {
  pub fn parent(&mut self) -> Option<TypesCommit<'_, R>> {
    match self.parents.fork() {
      StackStack::Nil => None,
      StackStack::Cons(patch, parents) => {
        Some(TypesCommit { patch, parents: parents.get(), repo: self.repo })
      }
    }
  }

  pub fn child<'b>(&'b mut self, patch: &'b mut TypesPatch) -> TypesCommit<'b, R> {
    TypesCommit {
      patch,
      parents: StackStack::Cons(self.patch, self.parents.borrow()),
      repo: self.repo,
    }
  }

  fn _find(&mut self, mut ty: Type) -> (Type, Type, TypeState) {
    let mut prev: Option<Type> = None;
    let mut init = ty;
    let state = loop {
      let node = match self.patch.nodes.get(&ty.idx()) {
        Some(&node) => node,
        None => {
          let (next, state) = self.parent().unwrap().find(ty);
          if let Some(prev) = prev {
            *self.patch.nodes.get_mut(&prev.idx()).unwrap() =
              TypeNode::Child(next.invert_if(prev.inv()));
          } else {
            init = next;
          }
          ty = next;
          match self.patch.nodes.get(&ty.idx()) {
            Some(&node) => node,
            None => break state,
          }
        }
      };
      match node {
        TypeNode::Root(state) => break state.invert_if(ty.inv()),
        TypeNode::Child(parent) => {
          prev = Some(ty);
          ty = parent.invert_if(ty.inv());
        }
      }
    };
    (init, ty, state)
  }

  pub fn state(&mut self, ty: Type) -> TypeState {
    self.find(ty).1
  }

  pub fn find(&mut self, ty: Type) -> (Type, TypeState) {
    let (init, root, state) = self._find(ty);
    self._compress(init, root);
    (root, state)
  }

  fn _compress(&mut self, mut ty: Type, root: Type) {
    while ty != root {
      let Some(TypeNode::Child(parent)) = self.patch.nodes.get_mut(&ty.idx()) else {
        unreachable!()
      };
      (*parent, ty) = (root.invert_if(ty.inv()), parent.invert_if(ty.inv()));
    }
  }
}

impl<'a> TypesCommit<'a> {
  pub fn new_var(&mut self) -> Type {
    let ty = self.repo.types.next();
    self.patch.new.push(ty);
    self.patch.nodes.insert(ty, TypeNode::Root(TypeState::Unknown { self_inv: false }));
    ty.into()
  }

  #[allow(clippy::new_ret_no_self)]
  pub fn new(&mut self, kind: TypeKind) -> Type {
    let kind = self.repo.kinds.insert(kind);
    let ty = self.repo.types.next();
    self.patch.new.push(ty);
    self.patch.nodes.insert(ty, TypeNode::Root(TypeState::Known(Inverted(false), kind)));
    ty.into()
  }

  pub fn unify(&mut self, a: Type, b: Type) -> UnifyResult {
    self.transaction(|mut commit| commit._unify(a, b))
  }

  fn transaction<T>(&mut self, f: impl FnOnce(TypesCommit<'_, Transaction<'_>>) -> T) -> T {
    let mut transaction =
      Transaction { kinds: &self.repo.kinds, dead_kinds: &mut self.repo._dead_kinds };
    let result =
      f(TypesCommit { patch: self.patch, parents: self.parents.fork(), repo: &mut transaction });
    for kind_id in self.repo._dead_kinds.drain(..) {
      self.repo.kinds.remove(kind_id);
    }
    result
  }
}

struct Transaction<'a> {
  kinds: &'a IdxSlab<TypeKindId, TypeKind>,
  dead_kinds: &'a mut Vec<TypeKindId>,
}

impl TypesCommit<'_, Transaction<'_>> {
  fn _unify(&mut self, a: Type, b: Type) -> UnifyResult {
    let (a, a_state) = self.find(a);
    let (b, b_state) = self.find(b);
    self._unify_roots(a, a_state, b, b_state)
  }

  fn _unify_roots(
    &mut self,
    a: Type,
    a_state: TypeState,
    b: Type,
    b_state: TypeState,
  ) -> UnifyResult {
    if a.idx() == b.idx() {
      return UnifyResult::from_bool(
        a.inverse() == b.inverse() || self._ensure(true, None, a, a_state),
      );
    }

    let swap = match (a_state, b_state) {
      (TypeState::Unknown { self_inv }, TypeState::Unknown { .. }) => self_inv,
      (TypeState::Unknown { self_inv }, TypeState::Known(..)) => {
        if !self._ensure(self_inv, Some(a.idx()), b, b_state) {
          return UnifyResult::Failure;
        }
        false
      }
      (TypeState::Known(..), TypeState::Unknown { self_inv }) => {
        if !self._ensure(self_inv, Some(b.idx()), a, a_state) {
          return UnifyResult::Failure;
        }
        true
      }
      (TypeState::Known(a_inv, a_kind), TypeState::Known(b_inv, b_kind)) => {
        let (result, sub_queries) =
          self.repo.kinds[a_kind].unify(&self.repo.kinds[b_kind], a_inv ^ b_inv);
        let result = result.and(UnifyResult::all(sub_queries.map(|(a, b)| self._unify(a, b))));
        if !result.is_success() {
          return result;
        }
        if self.patch.nodes.contains_key(&b.idx()) {
          self.repo.dead_kinds.push(b_kind);
        }
        false
      }
    };

    let (a, b) = if swap { (b, a) } else { (a, b) };

    self.patch.nodes.insert(a.idx(), TypeNode::Child(b.invert_if(a.inv())));

    UnifyResult::Success
  }

  fn _ensure(&mut self, self_inv: bool, var: Option<TypeIdx>, ty: Type, state: TypeState) -> bool {
    match state {
      TypeState::Known(_, kind) => {
        let kind = &self.repo.kinds[kind];
        (!self_inv || kind.self_dual())
          && kind
            .children()
            .map(|ty| {
              let (ty, state) = self.find(ty);
              var != Some(ty.idx()) && self._ensure(self_inv, var, ty, state)
            })
            .fold(true, |a, b| a & b)
      }
      TypeState::Unknown { self_inv: is_self_inv } => {
        if self_inv && !is_self_inv {
          self.patch.nodes.insert(ty.idx(), TypeNode::Root(TypeState::Unknown { self_inv: true }));
        }
        true
      }
    }
  }
}

#[derive(Debug, Clone)]
pub struct Delta {
  uprooted: BTreeSet<TypeIdx>,
}

impl TypesCommit<'_> {
  pub fn squash(&mut self, patch: &mut TypesPatch) -> Delta {
    let mut uprooted = patch.nodes.keys().copied().collect::<BTreeSet<_>>();
    let mut todo = uprooted.clone();
    while let Some(idx) = todo.pop_first() {
      let (mut ty, root, _) = self.child(patch)._find(idx.into());
      while ty != root && todo.remove(&ty.idx()) {
        let Some(TypeNode::Child(parent)) = patch.nodes.get_mut(&ty.idx()) else { unreachable!() };
        self.patch.nodes.insert(ty.idx(), TypeNode::Child(root.invert_if(ty.inv())));
        (*parent, ty) = (root.invert_if(ty.inv()), parent.invert_if(ty.inv()));
      }
      uprooted.insert(root.idx());
      todo.remove(&root.idx());
      if let Some(&node) = patch.nodes.get(&root.idx()) {
        self.patch.nodes.insert(ty.idx(), node);
      }
    }
    patch.nodes.clear();
    self.patch.new.reserve(patch.new.len());
    for idx in patch.new.drain(..) {
      uprooted.remove(&idx);
      self.patch.new.push(idx);
    }
    Delta { uprooted }
  }

  pub fn rebase<'d>(&mut self, parent_delta: &'d Delta) -> Option<Cow<'d, Delta>> {
    self.transaction(|mut self_| self_._rebase(parent_delta))
  }
}

impl TypesCommit<'_, Transaction<'_>> {
  fn _rebase<'d>(&mut self, parent_delta: &'d Delta) -> Option<Cow<'d, Delta>> {
    if self.patch.is_empty() {
      return Some(Cow::Borrowed(parent_delta));
    }
    let mut uprooted = BTreeSet::new();
    let mut equations = Vec::new();
    for &idx in &parent_delta.uprooted {
      if let Some(node) = self.patch.nodes.remove(&idx) {
        let TypeNode::Child(parent) = node else { unreachable!() };
        equations.push((Type::from(idx), parent));
      } else {
        uprooted.insert(idx);
      }
    }
    for (a, b) in equations {
      let (a, a_state) = self.find(a);
      let (b, b_state) = self.find(b);
      if self._unify_roots(a, a_state, b, b_state).is_success() {
        if a != b {
          uprooted.insert(a.idx());
          uprooted.insert(b.idx());
        }
      } else {
        return None;
      }
    }
    Some(Cow::Owned(Delta { uprooted }))
  }
}
