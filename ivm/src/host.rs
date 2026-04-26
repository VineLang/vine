use std::{any::TypeId, collections::HashMap};

use ivy::name::NameId;

use crate::{
  runtime::{
    Runtime,
    allocator::Allocator,
    ext::{ExtFnId, ExtTyId, Extrinsics},
    graft::Graft,
    heap::Heap,
  },
  util::Arena,
};

pub mod ext;

pub struct IVM<'ivm> {
  heaps: Arena<Heap>,
  pub(crate) grafts: Arena<[Graft<'ivm>]>,
  pub(crate) branches: Arena<Box<[*const Graft<'ivm>]>>,
}

impl<'ivm> IVM<'ivm> {
  #[allow(clippy::new_without_default)]
  pub fn new() -> Self {
    Self { heaps: Default::default(), grafts: Default::default(), branches: Default::default() }
  }
}

pub struct Host<'ivm> {
  pub(crate) ivm: &'ivm IVM<'ivm>,
  extrinsics: Extrinsics<'ivm>,
  pub(crate) ext_merge_lookup: HashMap<NameId, ExtFnId<'ivm>>,
  pub(crate) ext_split_lookup: HashMap<NameId, ExtFnId<'ivm>>,
  pub(crate) ext_types_lookup: HashMap<TypeId, ExtTyId<'ivm>>,
}

impl<'ivm> Host<'ivm> {
  pub fn new(ivm: &'ivm mut IVM<'ivm>) -> Self {
    Host {
      ivm,
      extrinsics: unsafe { Extrinsics::new() },
      ext_merge_lookup: Default::default(),
      ext_split_lookup: Default::default(),
      ext_types_lookup: Default::default(),
    }
  }

  pub fn heap(&self, heap: Box<Heap>) -> &'ivm mut Heap {
    self.ivm.heaps.push(heap)
  }

  pub fn init(&self, heap: &'ivm mut Heap) -> Runtime<'ivm, '_> {
    Runtime::new(Allocator::new(heap), &self.extrinsics)
  }
}
