#![warn(clippy::std_instead_of_core)]

use core::mem;
use std::time::Instant;

mod parallel;

pub mod addr;
pub mod bench;
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
  allocator::Allocator, bench::Bencher, ext::Extrinsics, flags::Flags, port::Port, stats::Stats,
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

  /// Used by the `ivm:io:bench` extrinsic function.
  pub(crate) benches: Bencher<'ivm>,
}

impl<'ivm, 'ext> Runtime<'ivm, 'ext> {
  pub(crate) fn new(alloc: Allocator<'ivm>, extrinsics: &'ext Extrinsics<'ivm>) -> Self {
    Self {
      alloc,
      extrinsics,
      alloc_pool: Vec::new(),
      registers: Vec::new(),
      benches: Bencher::default(),
      active_fast: Vec::new(),
      active_slow: Vec::new(),
      stats: Stats::default(),
      flags: Flags::default(),
    }
  }

  /// Normalize all nets in this IVM.
  pub fn normalize<H: Hooks>(&mut self, mut hooks: H) {
    let mut start = hooks.now();
    let mut stats = Vec::new();
    loop {
      hooks.tick(start, &mut self.stats);

      self.do_fast();

      if let Some((a, b)) = self.active_slow.pop() {
        self.interact(a, b);
      } else if !self.bench_check(&mut start, &mut stats, &mut hooks) {
        break;
      }
    }

    hooks.end(start, &mut self.stats);
  }

  /// Reduce all "fast" active pairs, returning the number of interactions.
  pub(crate) fn do_fast(&mut self) {
    while let Some((a, b)) = self.active_fast.pop() {
      self.interact(a, b);
    }
  }

  /// Normalize all nets in breadth-first traversal.
  ///
  /// This is useful to get the depth (longest critical path) of the computation
  /// to understand the parallelism of the program.
  pub fn normalize_breadth_first<H: Hooks>(&mut self, mut hooks: H) {
    let mut start = hooks.now();
    let mut work = vec![];
    let mut stats: Vec<(H::Instant, Stats)> = Vec::new();
    loop {
      hooks.tick(start, &mut self.stats);

      mem::swap(&mut work, &mut self.active_fast);
      work.append(&mut self.active_slow);
      if work.is_empty() && !self.bench_check(&mut start, &mut stats, &mut hooks) {
        break;
      }
      for (a, b) in work.drain(..) {
        self.interact(a, b);
      }
      self.stats.depth += 1;
    }

    hooks.end(start, &mut self.stats);
  }
}

pub trait Hooks {
  type Instant: Copy;

  fn now(&mut self) -> Self::Instant;
  fn tick(&mut self, _start: Self::Instant, _stats: &mut Stats);
  fn end(&mut self, _start: Self::Instant, _stats: &mut Stats);
}

impl Hooks for () {
  type Instant = Instant;

  fn now(&mut self) -> Self::Instant {
    Instant::now()
  }

  fn tick(&mut self, _start: Self::Instant, _stats: &mut Stats) {}

  fn end(&mut self, start: Self::Instant, stats: &mut Stats) {
    stats.time_clock += start.elapsed();
  }
}
