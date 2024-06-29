use std::time::Instant;

use crate::{addr::Addr, ext::ExtVal, global::Global, heap::Heap, port::Port, stats::Stats};

/// An Interaction Virtual Machine.
pub struct IVM<'ivm> {
  /// Execution statistics of this IVM.
  pub stats: Stats,

  /// The heap memory backing this IVM.
  pub(crate) heap: &'ivm Heap,
  /// The head pointer of the allocator's freelist.
  pub(crate) alloc_head: Addr,
  /// The next unallocated index in the `heap`.
  pub(crate) alloc_next: usize,

  /// Active pairs that should be "fast" to process (generally, those which do
  /// not allocate memory).
  pub(crate) active_fast: Vec<(Port<'ivm>, Port<'ivm>)>,
  /// Active pairs that may be "slow" to process (generally, those which may
  /// allocate memory).
  pub(crate) active_slow: Vec<(Port<'ivm>, Port<'ivm>)>,

  pub(crate) inert: Vec<(Port<'ivm>, Port<'ivm>)>,

  /// Used by [`IVM::execute`].
  pub(crate) registers: Vec<Option<Port<'ivm>>>,
}

impl<'ivm> IVM<'ivm> {
  /// Creates a new IVM with a given heap.
  pub fn new(heap: &'ivm Heap) -> Self {
    Self {
      heap,
      alloc_next: 0,
      alloc_head: Addr::NULL,
      registers: Vec::new(),
      active_fast: Vec::new(),
      active_slow: Vec::new(),
      inert: Vec::new(),
      stats: Stats::default(),
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
    loop {
      while let Some((a, b)) = self.active_fast.pop() {
        self.interact(a, b)
      }
      if let Some((a, b)) = self.active_slow.pop() {
        self.interact(a, b)
      } else {
        break;
      }
    }
    self.stats.time_total += start.elapsed();
  }
}
