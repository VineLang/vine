#![warn(clippy::std_instead_of_core)]

use core::mem;
use std::{collections::BTreeMap, time::Instant};

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
  allocator::Allocator,
  ext::{ExtTy, Extrinsics},
  flags::Flags,
  port::Port,
  stats::Stats,
  wire::Wire,
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

  /// Benches to perform. Once no more work can be done, a bench will be
  /// popped and intermediate stats will be collected.
  ///
  /// Pushed to by the `ivm:io:bench` extrinsic function.
  pub(crate) benches_todo: Vec<Bench<'ivm>>,
  /// Benches that have been performed. Subsequent calls to `ivm:io:bench`
  /// can mutate these collected stats further by referring to the same key.
  pub(crate) benches: BTreeMap<String, Stats>,
  /// Statistics stashed during a bench.
  stashed_stats: Option<Stats>,
}

impl<'ivm, 'ext> Runtime<'ivm, 'ext> {
  pub(crate) fn new(alloc: Allocator<'ivm>, extrinsics: &'ext Extrinsics<'ivm>) -> Self {
    Self {
      alloc,
      extrinsics,
      alloc_pool: Vec::new(),
      registers: Vec::new(),
      benches_todo: Vec::new(),
      benches: BTreeMap::new(),
      stashed_stats: None,
      active_fast: Vec::new(),
      active_slow: Vec::new(),
      stats: Stats::default(),
      flags: Flags::default(),
    }
  }

  /// Normalize all nets in this IVM.
  pub fn normalize<H: Hooks>(&mut self, mut hooks: H) {
    let start = hooks.now();
    let mut bench: Option<BenchCont<H::Instant>> = None;
    loop {
      hooks.tick(&start, &mut self.stats);

      self.do_fast();

      if let Some((a, b)) = self.active_slow.pop() {
        self.interact(a, b);
      } else if !self.bench_check(&mut bench, &mut hooks) {
        break;
      }
    }

    hooks.end(&start, &mut self.stats);
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
    let start = hooks.now();
    let mut work = vec![];
    let mut bench: Option<BenchCont<H::Instant>> = None;
    loop {
      hooks.tick(&start, &mut self.stats);

      mem::swap(&mut work, &mut self.active_fast);
      work.append(&mut self.active_slow);
      if work.is_empty() && !self.bench_check(&mut bench, &mut hooks) {
        break;
      }
      for (a, b) in work.drain(..) {
        self.interact(a, b);
      }
      self.stats.depth += 1;
    }

    hooks.end(&start, &mut self.stats);
  }

  pub(crate) fn push_stats(&mut self) {
    assert!(self.stashed_stats.replace(self.stats).is_none());
    self.stats = Stats::default();
  }

  pub(crate) fn pop_stats(&mut self) -> Stats {
    let stats = self.stats;
    self.stats += self.stashed_stats.take().unwrap();
    stats
  }

  fn bench_check<H: Hooks>(
    &mut self,
    bench: &mut Option<BenchCont<'ivm, H::Instant>>,
    hooks: &mut H,
  ) -> bool {
    if let Some(cont) = bench.take() {
      self.bench_resume(cont, hooks);
      return true;
    }

    if let Some(cont) = self.bench_start(hooks) {
      *bench = Some(cont);
      return true;
    }

    false
  }

  fn bench_start<H: Hooks>(&mut self, hooks: &mut H) -> Option<BenchCont<'ivm, H::Instant>> {
    let Bench { key, start, resume, n32 } = self.benches_todo.pop()?;
    let now = hooks.now();
    self.push_stats();
    self.link_wire(start, Port::new_ext_val(n32.wrap_static(1)));
    Some(BenchCont::new(key, now, resume, n32))
  }

  fn bench_resume<H: Hooks>(&mut self, cont: BenchCont<'ivm, H::Instant>, hooks: &mut H) {
    let BenchCont { key, now, resume, n32 } = cont;
    let stats = self.pop_stats();
    let prev = self.benches.entry(key).or_default();
    *prev += stats;
    hooks.end(&now, prev);
    self.link_wire(resume, Port::new_ext_val(n32.wrap_static(1)));
  }
}

pub(crate) struct Bench<'ivm> {
  key: String,
  start: Wire<'ivm>,
  resume: Wire<'ivm>,
  n32: ExtTy<'ivm, u32>,
}

impl<'ivm> Bench<'ivm> {
  pub(crate) fn new(
    key: String,
    start: Wire<'ivm>,
    resume: Wire<'ivm>,
    n32: ExtTy<'ivm, u32>,
  ) -> Self {
    Self { key, start, resume, n32 }
  }
}

struct BenchCont<'ivm, I> {
  key: String,
  now: I,
  resume: Wire<'ivm>,
  n32: ExtTy<'ivm, u32>,
}

impl<'ivm, I> BenchCont<'ivm, I> {
  fn new(key: String, now: I, resume: Wire<'ivm>, n32: ExtTy<'ivm, u32>) -> Self {
    Self { key, now, resume, n32 }
  }
}

pub trait Hooks {
  type Instant;

  fn now(&mut self) -> Self::Instant;
  fn tick(&mut self, _start: &Self::Instant, _stats: &mut Stats);
  fn end(&mut self, _start: &Self::Instant, _stats: &mut Stats);
}

impl Hooks for () {
  type Instant = Instant;

  fn now(&mut self) -> Self::Instant {
    Instant::now()
  }

  fn tick(&mut self, _start: &Self::Instant, _stats: &mut Stats) {}

  fn end(&mut self, start: &Self::Instant, stats: &mut Stats) {
    stats.time_clock += start.elapsed();
  }
}
