use std::{collections::BTreeMap, ops::BitAnd};

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  ast::{Flex, Ident, TypeParam},
  chart::{AdtId, TraitDefId, TypeDefId, ValueDefId},
  diag::ErrorGuaranteed,
  tir::ClosureId,
};

new_idx!(pub Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind<'core> {
  Opaque(TypeDefId),
  Tuple(Vec<Type>),
  Object(BTreeMap<Ident<'core>, Type>),
  Adt(AdtId, Vec<Type>),
  Ref(Type),
  Inverse(Type),
  Trait(TraitDefId, Vec<Type>),
  Closure(ValueDefId, ClosureId, Flex),
  Param(usize),
  Never,
  Error(ErrorGuaranteed),
}

#[derive(Debug)]
enum TypeNode<'core> {
  Root(Option<TypeKind<'core>>, usize),
  Child(Type),
}

#[derive(Debug)]
pub struct Types<'core> {
  types: IdxVec<Type, TypeNode<'core>>,
}

impl<'core> Types<'core> {
  pub fn new(&mut self, kind: TypeKind<'core>) -> Type {
    self.types.push(TypeNode::Root(Some(kind), 1))
  }

  pub fn new_var(&mut self) -> Type {
    self.types.push(TypeNode::Root(None, 1))
  }

  pub fn error(&mut self, err: ErrorGuaranteed) -> Type {
    todo!()
  }

  pub fn inverse(&mut self, _: Type) -> Type {
    todo!()
  }

  pub fn nil(&mut self) -> Type {
    todo!()
  }

  pub fn unify(&mut self, a: Type, b: Type) -> bool {
    let a = self.find(a);
    let b = self.find(b);

    if a == b {
      return true;
    }

    let (a_node, b_node) = self.types.get2_mut(a, b).unwrap();
    let TypeNode::Root(a_kind, a_size) = &mut *a_node else { unreachable!() };
    let TypeNode::Root(b_kind, b_size) = &mut *b_node else { unreachable!() };

    if a_kind.is_none() || b_kind.is_none() {
      let kind = a_kind.take().or(b_kind.take());
      if a_size > b_size {
        *a_kind = kind;
        *a_size += *b_size;
        *b_node = TypeNode::Child(a);
      } else {
        *b_kind = kind;
        *b_size += *a_size;
        *a_node = TypeNode::Child(b);
      }
      return true;
    }

    match (a_kind.as_mut().unwrap(), b_kind.as_mut().unwrap()) {
      (TypeKind::Error(_), _) | (_, TypeKind::Error(_)) => todo!(),
      (TypeKind::Opaque(i), TypeKind::Opaque(j)) => *i == *j,
      (TypeKind::Param(i), TypeKind::Param(j)) => *i == *j,
      (TypeKind::Tuple(a), TypeKind::Tuple(b)) if a.len() == b.len() => {
        a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)).fold(true, BitAnd::bitand)
      }
      (TypeKind::Object(a), TypeKind::Object(b)) if a.len() == b.len() => a
        .iter()
        .map(|(k, a)| if let Some(b) = b.get(k) { self.unify(*a, *b) } else { false })
        .fold(true, BitAnd::bitand),
      (TypeKind::Adt(i, a), TypeKind::Adt(j, b)) if *i == *j && a.len() == b.len() => {
        a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)).fold(true, BitAnd::bitand)
      }
      (TypeKind::Ref(a), TypeKind::Ref(b)) => self.unify(*a, *b),
      (TypeKind::Inverse(a), TypeKind::Inverse(b)) => self.unify(*a, *b),
      (TypeKind::Trait(i, a), TypeKind::Trait(j, b)) if *i == *j && a.len() == b.len() => {
        a.iter().zip(b).map(|(a, b)| self.unify(*a, *b)).fold(true, BitAnd::bitand)
      }
      // TypeKind::Closure(value_def_id, closure_id, flex) => todo!(),
      (TypeKind::Never, TypeKind::Never) => todo!(),
      _ => false,
    };

    todo!()
  }

  fn find(&mut self, ty: Type) -> Type {
    let mut cur = ty;
    while let TypeNode::Child(parent) = self.types[cur] {
      cur = parent;
    }
    let root = cur;
    let mut cur = ty;
    while let TypeNode::Child(parent) = &mut self.types[cur] {
      cur = *parent;
      *parent = root;
    }
    ty
  }

  pub fn show(&self, ty: Type) -> String {
    todo!()
  }

  pub(crate) fn kind(&self, ty: Type) -> &TypeKind {
    todo!()
  }
}
