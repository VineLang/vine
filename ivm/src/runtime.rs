#![warn(clippy::std_instead_of_core)]

use core::mem;
use std::time::Instant;

mod parallel;

pub mod addr;
pub mod ext;
pub mod flags;
pub mod graft;
pub mod heap;
pub mod port;
pub mod stats;
pub mod wire;
pub mod word;

pub(crate) mod allocator;
mod interact;

use crate::runtime::{
  allocator::Allocator, ext::Extrinsics, flags::Flags, port::Port, stats::Stats,
};

pub struct Runtime<'ivm, 'ext> {
  /// Execution statistics of this runtime.
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

  /// Used by [`IVM::execute`].
  pub(crate) registers: Vec<Option<Port<'ivm>>>,
}

impl<'ivm, 'ext> Runtime<'ivm, 'ext> {
  pub(crate) fn new(alloc: Allocator<'ivm>, extrinsics: &'ext Extrinsics<'ivm>) -> Self {
    Self {
      alloc,
      extrinsics,
      alloc_pool: Vec::new(),
      registers: Vec::new(),
      active_fast: Vec::new(),
      active_slow: Vec::new(),
      stats: Stats::default(),
      flags: Flags::default(),
    }
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

  /// Normalize all nets in breadth-first traversal.
  ///
  /// This is useful to get the depth (longest critical path) of the computation
  /// to understand the parallelism of the program.
  pub fn normalize_breadth_first(&mut self) {
    let start = Instant::now();
    let mut work = vec![];
    loop {
      mem::swap(&mut work, &mut self.active_fast);
      work.append(&mut self.active_slow);
      if work.is_empty() {
        break;
      }
      for (a, b) in work.drain(..) {
        self.interact(a, b);
      }
      self.stats.depth += 1;
    }
    self.stats.time_clock += start.elapsed();
  }
}
