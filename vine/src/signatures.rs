use std::collections::HashMap;

use vine_util::idx::IdxVec;

use crate::{
  ast::Ident,
  chart::{
    ChartCheckpoint, EnumId, GenericsId, ImplDefId, StructId, SubitemId, TraitDefId, TypeDefId,
    ValueDefId, VariantId,
  },
  types::{ImplType, Type, TypeCtx},
};

#[derive(Debug, Default)]
pub struct Signatures<'core> {
  pub values: IdxVec<ValueDefId, TypeCtx<'core, ValueSig>>,
  pub types: IdxVec<TypeDefId, Option<TypeCtx<'core, TypeSig>>>,
  pub traits: IdxVec<TraitDefId, TypeCtx<'core, TraitSig<'core>>>,
  pub impls: IdxVec<ImplDefId, TypeCtx<'core, ImplSig>>,
  pub generics: IdxVec<GenericsId, TypeCtx<'core, GenericsSig>>,
  pub structs: IdxVec<StructId, TypeCtx<'core, StructSig>>,
  pub enums: IdxVec<EnumId, TypeCtx<'core, EnumSig>>,
}

#[derive(Debug)]
pub struct ValueSig {
  pub ty: Type,
}

#[derive(Debug)]
pub struct TypeSig {
  pub ty: Type,
}

#[derive(Debug)]
pub struct TraitSig<'core> {
  pub lookup: HashMap<Ident<'core>, SubitemId>,
  pub subitem_types: IdxVec<SubitemId, Type>,
}

#[derive(Debug)]
pub struct ImplSig {
  pub ty: ImplType,
}

#[derive(Debug)]
pub struct GenericsSig {
  pub impl_params: Vec<ImplType>,
}

#[derive(Debug)]
pub struct StructSig {
  pub data: Type,
}

#[derive(Debug)]
pub struct EnumSig {
  pub variant_data: IdxVec<VariantId, Option<Type>>,
}

impl<'core> Signatures<'core> {
  pub fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    self.values.truncate(checkpoint.values.0);
    self.types.truncate(checkpoint.types.0);
    self.traits.truncate(checkpoint.traits.0);
    self.impls.truncate(checkpoint.impls.0);
    self.generics.truncate(checkpoint.generics.0);
    self.structs.truncate(checkpoint.structs.0);
    self.enums.truncate(checkpoint.enums.0);
  }
}
