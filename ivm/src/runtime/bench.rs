use core::mem;

use vine_util::{idx::IdxVec, new_idx};

use crate::runtime::{Hooks, Runtime, ext::ExtTy, port::Port, stats::Stats, wire::Wire};

impl<'ivm, 'ext> Runtime<'ivm, 'ext> {
  pub(crate) fn push_bench(
    &mut self,
    key: String,
    start: Wire<'ivm>,
    resume: Wire<'ivm>,
    n32: ExtTy<'ivm, u32>,
  ) {
    self.benches.push(Request { key, start, resume, n32 })
  }

  pub(crate) fn bench_check<H: Hooks>(
    &mut self,
    start: &mut H::Instant,
    stats: &mut Vec<(H::Instant, Stats)>,
    hooks: &mut H,
  ) -> bool {
    if let Some((n32, start_w)) = self.benches.start() {
      stats.push((*start, self.stats));
      (*start, self.stats) = (hooks.now(), Stats::default());
      self.link_wire(start_w, Port::new_ext_val(n32.wrap_static(1)));
      return true;
    }

    if let Some((n32, resume_w)) = self.benches.resume(self.stats) {
      hooks.end(*start, &mut self.stats);
      let (prev_start, prev_stats) = stats.pop().unwrap();
      *start = prev_start;
      self.stats += prev_stats;
      self.link_wire(resume_w, Port::new_ext_val(n32.wrap_static(1)));
      return true;
    }

    false
  }
}

pub type Benches = IdxVec<BenchId, Bench>;

#[derive(Default)]
pub(crate) struct Bencher<'ivm> {
  benches: Benches,
  current: Option<Resume<'ivm>>,
  todos: Vec<Request<'ivm>>,
  stack: Vec<(Option<Resume<'ivm>>, Vec<Request<'ivm>>)>,
}

impl<'ivm> Bencher<'ivm> {
  pub(crate) fn into_inner(self) -> Benches {
    assert!(self.stack.is_empty());
    assert!(self.todos.is_empty());
    assert!(self.current.is_none());
    self.benches
  }

  fn push(&mut self, req: Request<'ivm>) {
    eprintln!("request {:?}", req.key);
    self.todos.push(req);
    eprintln!("todos len: {}", self.todos.len());
  }

  fn start(&mut self) -> Option<Gate<'ivm>> {
    let Request { key, start, resume, n32 } = self.todos.pop()?;
    eprintln!("starting {key:?}");
    let id = self.benches.push(Bench::new(key));

    let parent = self.current.replace(Resume { id, resume, n32 });
    if let Some(current) = &parent {
      self.benches[id].parent = Some(current.id);
      self.benches[current.id].children.push(id);
    }

    let mut todos = Vec::new();
    mem::swap(&mut self.todos, &mut todos);
    self.stack.push((parent, todos));

    eprintln!("todos len: {}", self.todos.len());

    Some((n32, start))
  }

  fn resume(&mut self, stats: Stats) -> Option<Gate<'ivm>> {
    let Resume { id, resume, n32 } = self.current.take()?;
    eprintln!("resuming {:?}", self.benches[id].key);
    self.benches[id].stats = stats;

    if let Some((prev, todos)) = self.stack.pop() {
      assert!(self.todos.is_empty());
      self.todos = todos;
      self.current = prev;
    }

    Some((n32, resume))
  }
}
pub(crate) type Gate<'ivm> = (ExtTy<'ivm, u32>, Wire<'ivm>);

new_idx!(pub BenchId);

pub struct Bench {
  pub key: String,
  pub stats: Stats,
  pub parent: Option<BenchId>,
  pub children: Vec<BenchId>,
}

impl Bench {
  fn new(key: String) -> Self {
    Self { key, stats: Stats::default(), parent: None, children: Vec::new() }
  }
}

struct Request<'ivm> {
  key: String,
  start: Wire<'ivm>,
  resume: Wire<'ivm>,
  n32: ExtTy<'ivm, u32>,
}

struct Resume<'ivm> {
  id: BenchId,
  resume: Wire<'ivm>,
  n32: ExtTy<'ivm, u32>,
}
