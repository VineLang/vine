use vine_util::idx::IdxVec;

use crate::{
  chart::{
    EnumId, GenericsId, ImplDefId, StructId, SubitemId, TraitDefId, TypeDefId, ValueDefId,
    VariantId,
  },
  types::{ImplType, Type, Types},
};

#[derive(Debug)]
pub struct Signatures<'core> {
  pub values: IdxVec<ValueDefId, ValueSig<'core>>,
  pub types: IdxVec<TypeDefId, TypeSig<'core>>,
  pub traits: IdxVec<TraitDefId, TraitSig<'core>>,
  pub impls: IdxVec<ImplDefId, ImplSig<'core>>,
  pub generics: IdxVec<GenericsId, GenericsSig<'core>>,
  pub structs: IdxVec<StructId, StructSig<'core>>,
  pub enums: IdxVec<EnumId, EnumSig<'core>>,
}

#[derive(Debug)]
pub struct ValueSig<'core> {
  pub types: Types<'core>,
  pub kind: ValueSigKind,
}

#[derive(Debug)]
pub enum ValueSigKind {
  Const(Type),
  Fn(Vec<Type>, Type),
  Struct(StructId, Type),
  Enum(EnumId, VariantId, Option<Type>),
}

#[derive(Debug)]
pub struct TypeSig<'core> {
  pub types: Types<'core>,
  pub ty: Type,
}

#[derive(Debug)]
pub struct TraitSig<'core> {
  pub types: Types<'core>,
  pub subitems: IdxVec<SubitemId, TraitSubitemSig>,
}

#[derive(Debug)]
pub enum TraitSubitemSig {
  Value(Type),
  Fn(Vec<Type>, Type),
}

#[derive(Debug)]
pub struct ImplSig<'core> {
  pub types: Types<'core>,
  pub ty: ImplType,
}

#[derive(Debug)]
pub struct GenericsSig<'core> {
  pub types: Types<'core>,
  pub impl_params: Vec<ImplType>,
}

#[derive(Debug)]
pub struct StructSig<'core> {
  pub types: Types<'core>,
  pub data: Type,
}

#[derive(Debug)]
pub struct EnumSig<'core> {
  pub types: Types<'core>,
  pub variant_data: IdxVec<VariantId, Option<Type>>,
}
