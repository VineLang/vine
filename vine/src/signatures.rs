use vine_util::idx::IdxVec;

use crate::{
  chart::{AdtId, GenericsId, ImplDefId, SubitemId, TraitDefId, TypeDefId, ValueDefId},
  types::{ImplType, Type, Types},
};

pub struct Signatures<'core> {
  pub values: IdxVec<ValueDefId, ValueSig<'core>>,
  pub types: IdxVec<TypeDefId, TypeSig<'core>>,
  pub traits: IdxVec<TraitDefId, TraitSig<'core>>,
  pub impls: IdxVec<ImplDefId, ImplSig<'core>>,
  pub generics: IdxVec<GenericsId, GenericsSig<'core>>,
  pub adts: IdxVec<AdtId, AdtSig<'core>>,
}

pub struct ValueSig<'core> {
  pub types: Types<'core>,
  pub ty: Type,
}

pub struct TypeSig<'core> {
  pub types: Types<'core>,
  pub ty: Type,
}

pub struct TraitSig<'core> {
  pub types: Types<'core>,
  pub subitems: IdxVec<SubitemId, TraitSubitemSig>,
}

pub enum TraitSubitemSig {
  Value(Type),
  Fn(Vec<Type>, Type),
}

pub struct ImplSig<'core> {
  pub types: Types<'core>,
  pub ty: ImplType,
}

pub struct GenericsSig<'core> {
  pub types: Types<'core>,
  pub impl_params: Vec<ImplType>,
}

pub struct AdtSig<'core> {
  pub types: Types<'core>,
  pub fields: Type,
}
