use core::mem;
use std::time::Instant;

use crate::{
  allocator::Allocator,
  ext::{Extrinsics, OpaqueExtFn},
  flags::Flags,
  heap::Heap,
  port::Port,
  stats::Stats,
};

/// An Interaction Virtual Machine.
pub struct IVM<'ivm, 'ext> {
  /// Execution statistics of this IVM.
  pub stats: Stats,

  /// Error flags set during interactions.
  pub flags: Flags,

  pub(crate) extrinsics: &'ext Extrinsics<'ivm>,

  pub(crate) alloc: Allocator<'ivm>,
  pub(crate) alloc_pool: Vec<Allocator<'ivm>>,

  /// Active pairs that should be "fast" to process (generally, those which do
  /// not allocate memory).
  pub(crate) active_fast: Vec<(Port<'ivm>, Port<'ivm>)>,
  /// Active pairs that may be "slow" to process (generally, those which may
  /// allocate memory).
  pub(crate) active_slow: Vec<(Port<'ivm>, Port<'ivm>)>,

  pub inert_links: Vec<(Port<'ivm>, Port<'ivm>)>,
  pub inert_nodes: Vec<(OpaqueExtFn, Port<'ivm>, Port<'ivm>, Port<'ivm>)>,

  /// Used by [`IVM::execute`].
  pub(crate) registers: Vec<Option<Port<'ivm>>>,
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
      extrinsics,
      alloc_pool: Vec::new(),
      registers: Vec::new(),
      active_fast: Vec::new(),
      active_slow: Vec::new(),
      inert_links: Vec::new(),
      inert_nodes: Vec::new(),
      stats: Stats::default(),
      flags: Flags::default(),
    }
  }

  /// Normalize all nets in this IVM.
  pub fn normalize(&mut self) {
    #[cfg(not(target_arch = "wasm32"))]
    let start = Instant::now();
    while self.normalize_batch() > 0 {}
    #[cfg(not(target_arch = "wasm32"))]
    {
      self.stats.time_clock += start.elapsed();
    }
  }

  pub fn normalize_batch(&mut self) -> usize {
    let mut interactions = self.active_fast.len();
    self.do_fast();
    if let Some((a, b)) = self.active_slow.pop() {
      self.interact(a, b);
      interactions += 1;
    }
    interactions
  }

  pub(crate) fn do_fast(&mut self) {
    while let Some((a, b)) = self.active_fast.pop() {
      self.interact(a, b)
    }
  }

  /// Normalize all nets in breadth-first traversal.
  ///
  /// This is useful to get the depth (longest critical path) of the computation
  /// to understand the parallelism of the program.
  pub fn normalize_breadth_first(&mut self) {
    #[cfg(not(target_arch = "wasm32"))]
    let start = Instant::now();
    let mut work = vec![];
    while self.normalize_breadth_first_batch(&mut work) > 0 {}
    #[cfg(not(target_arch = "wasm32"))]
    {
      self.stats.time_clock += start.elapsed();
    }
  }

  pub fn normalize_breadth_first_batch(
    &mut self,
    work: &mut Vec<(Port<'ivm>, Port<'ivm>)>,
  ) -> usize {
    mem::swap(work, &mut self.active_fast);
    work.append(&mut self.active_slow);
    if work.is_empty() {
      return 0;
    }
    self.stats.depth += 1;
    let interactions = work.len();
    for (a, b) in work.drain(..) {
      self.interact(a, b);
    }
    interactions
  }

  /// Returns whether any active pairs exist.
  pub fn is_finished(&self) -> bool {
    self.active_fast.is_empty() && self.active_slow.is_empty()
  }
}
