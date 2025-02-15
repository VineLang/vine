use std::time::Instant;

use crate::{
  addr::Addr, allocator::Allocator, ext::ExtVal, global::Global, heap::Heap, port::Port,
  stats::Stats,
};

/// An Interaction Virtual Machine.
pub struct IVM<'ivm> {
  /// Execution statistics of this IVM.
  pub stats: Stats,

  pub(crate) alloc: Allocator<'ivm>,
  pub(crate) alloc_pool: Vec<Allocator<'ivm>>,

  pub(crate) active: Vec<(Port<'ivm>, Port<'ivm>)>,

  /// Used by [`IVM::execute`].
  pub(crate) mapping: Vec<Addr>,
}

impl<'ivm> IVM<'ivm> {
  /// Creates a new IVM with a given heap.
  pub fn new(heap: &'ivm Heap) -> Self {
    Self::new_from_allocator(Allocator::new(heap))
  }

  pub(crate) fn new_from_allocator(alloc: Allocator<'ivm>) -> Self {
    Self {
      alloc,
      alloc_pool: Vec::new(),
      active: Vec::new(),
      stats: Stats::default(),
      mapping: Vec::new(),
    }
  }

  /// Boots this IVM from the `main` global, connecting it to an IO handle.
  ///
  /// This does not start any processing; [`IVM::normalize`] must be called to
  /// do that.
  pub fn boot(&mut self, main: &'ivm Global<'ivm>) {
    self.link(Port::new_global(main), Port::new_ext_val(ExtVal::IO));
  }

  /// Normalize all nets in this IVM.
  pub fn normalize(&mut self) {
    let start = Instant::now();
    while let Some((a, b)) = self.active.pop() {
      self.interact(a, b)
    }
    self.stats.time_clock += start.elapsed();
  }
}
