use std::time::Instant;

use crate::{
  allocator::Allocator,
  ext::{ExtVal, Extrinsics},
  global::Global,
  heap::Heap,
  port::Port,
  stats::Stats,
};

/// An Interaction Virtual Machine.
pub struct IVM<'ivm, 'ext> {
  /// Execution statistics of this IVM.
  pub stats: Stats,

  pub(crate) alloc: Allocator<'ivm>,
  pub(crate) alloc_pool: Vec<Allocator<'ivm>>,

  /// Active pairs that should be "fast" to process (generally, those which do
  /// not allocate memory).
  pub(crate) active_fast: Vec<(Port<'ivm>, Port<'ivm>)>,
  /// Active pairs that may be "slow" to process (generally, those which may
  /// allocate memory).
  pub(crate) active_slow: Vec<(Port<'ivm>, Port<'ivm>)>,

  /// Used by [`IVM::execute`].
  pub(crate) registers: Vec<Option<Port<'ivm>>>,

  pub(crate) extrinsics: &'ext Extrinsics<'ivm>,
}

impl<'ivm, 'ext> IVM<'ivm, 'ext> {
  /// Creates a new IVM with a given heap.
  pub fn new(heap: &'ivm Heap, extrinsics: &'ext Extrinsics<'ivm>) -> Self {
    Self::new_from_allocator(Allocator::new(heap), extrinsics)
  }

  pub(crate) fn new_from_allocator(
    alloc: Allocator<'ivm>,
    extrinsics: &'ext Extrinsics<'ivm>,
  ) -> Self {
    Self {
      alloc,
      alloc_pool: Vec::new(),
      registers: Vec::new(),
      active_fast: Vec::new(),
      active_slow: Vec::new(),
      stats: Stats::default(),
      extrinsics,
    }
  }

  /// Boots this IVM from the `main` global, connecting it to `root` (usually an
  /// IO handle).
  ///
  /// This does not start any processing; [`IVM::normalize`] must be called to
  /// do that.
  pub fn boot(&mut self, main: &'ivm Global<'ivm>, root: ExtVal<'ivm>) {
    self.link(Port::new_global(main), Port::new_ext_val(root));
  }

  /// Normalize all nets in this IVM.
  pub fn normalize(&mut self) {
    let start = Instant::now();
    loop {
      self.do_fast();
      if let Some((a, b)) = self.active_slow.pop() {
        self.interact(a, b)
      } else {
        break;
      }
    }
    self.stats.time_clock += start.elapsed();
  }

  pub(crate) fn do_fast(&mut self) {
    while let Some((a, b)) = self.active_fast.pop() {
      self.interact(a, b)
    }
  }
}
