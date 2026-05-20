use core::mem;

use vine_util::{idx::IdxVec, new_idx};

use crate::runtime::{Hooks, Runtime, ext::ExtTy, port::Port, stats::Stats, wire::Wire};

impl<'ivm, 'ext> Runtime<'ivm, 'ext> {
  pub(crate) fn push_bench(
    &mut self,
    key: String,
    enter: Wire<'ivm>,
    exit: Wire<'ivm>,
    n32: ExtTy<'ivm, u32>,
  ) {
    self.bencher.todo.push(Request { key, enter, exit, n32 })
  }

  pub(crate) fn bench_check<H: Hooks>(
    &mut self,
    start: &mut H::Instant,
    cont: &mut Option<Cont<'ivm, H::Instant>>,
    hooks: &mut H,
  ) -> bool {
    if let Some(Cont { id, prev_start, prev_stats, exit, n32 }) = cont.take() {
      hooks.end(start, &mut self.stats);
      self.bencher.benches[id].stats = self.stats;
      *start = prev_start;
      self.stats += prev_stats;
      self.link_wire(exit, Port::new_ext_val(n32.wrap_static(1)));
      return true;
    }

    if let Some(Request { key, enter, exit, n32 }) = self.bencher.todo.pop() {
      let id = self.bencher.benches.push(Bench::new(key));
      let prev_start = mem::replace(start, hooks.now());
      let prev_stats = mem::take(&mut self.stats);
      assert!(cont.replace(Cont { id, prev_start, prev_stats, exit, n32 }).is_none());
      self.link_wire(enter, Port::new_ext_val(n32.wrap_static(1)));
      return true;
    }

    false
  }
}

pub type Benches = IdxVec<BenchId, Bench>;

new_idx!(pub BenchId);

#[derive(Default)]
pub(crate) struct Bencher<'ivm> {
  pub(crate) benches: IdxVec<BenchId, Bench>,
  pub(crate) todo: Vec<Request<'ivm>>,
}

pub struct Bench {
  pub name: String,
  pub stats: Stats,
}

impl Bench {
  fn new(name: String) -> Self {
    Self { name, stats: Stats::default() }
  }
}

pub(crate) struct Request<'ivm> {
  key: String,
  enter: Wire<'ivm>,
  exit: Wire<'ivm>,
  n32: ExtTy<'ivm, u32>,
}

pub struct Cont<'ivm, I> {
  id: BenchId,
  prev_start: I,
  prev_stats: Stats,
  exit: Wire<'ivm>,
  n32: ExtTy<'ivm, u32>,
}
