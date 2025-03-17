use std::collections::BTreeMap;

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  ast::{Flex, Ident},
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
  Fn(Vec<Type>, Type),
  Ref(Type),
  Inverse(Type),
  Trait(TraitDefId, Vec<Type>),
  Closure(ValueDefId, ClosureId, Flex),
  Param(usize),
  Never,
  Error(ErrorGuaranteed),
}

#[derive(Debug)]
pub struct Types<'core> {
  types: IdxVec<Type, TypeKind<'core>>,
}

impl<'core> Types<'core> {
  pub fn new(&mut self, kind: TypeKind<'core>) -> Type {
    todo!()
  }

  pub fn new_var(&mut self) -> Type {
    todo!()
  }

  pub fn error(&mut self, _: ErrorGuaranteed) -> Type {
    todo!()
  }

  pub fn inverse(&mut self, _: Type) -> Type {
    todo!()
  }

  pub fn nil(&mut self) -> Type {
    todo!()
  }

  pub fn unify(&mut self, a: Type, b: Type) -> bool {
    todo!()
  }

  pub fn show(&self, ty: Type) -> String {
    todo!()
  }

  pub(crate) fn kind(&self, ty: Type) -> &TypeKind {
    todo!()
  }
}
