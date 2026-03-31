use std::{
  io::{self, Read, Write},
  sync::{Mutex, MutexGuard},
};

use ivm::{
  IVM,
  ext::Extrinsics,
  flags::Flags,
  heap::Heap,
  port::{Port, Tag},
  stats::Stats,
};

use crate::{ast::Nets, host::Host};

#[derive(Default)]
pub struct Run {
  pub workers: usize,
  pub breadth_first: bool,
  pub heap: Option<usize>,
  pub args: Vec<String>,
}

impl Run {
  pub fn run<R: Read, W: Write>(
    &self,
    nets: &Nets,
    io_input_fn: impl Copy + Fn() -> R + Send + Sync,
    io_output_fn: impl Copy + Fn() -> W + Send + Sync,
  ) -> RunResult {
    let mut host = &mut Host::default();
    let heap = self.heap();
    let extrinsics = &mut Extrinsics::default();
    let (mut ivm, root) = self.boot(&mut host, &heap, extrinsics, io_input_fn, io_output_fn, nets);

    if self.breadth_first {
      ivm.normalize_breadth_first();
    } else if self.workers > 0 {
      ivm.normalize_parallel(self.workers)
    } else {
      ivm.normalize();
    }

    let out = ivm.follow(root);
    RunResult {
      stats: ivm.stats,
      flags: ivm.flags,
      no_io: out.tag() != Tag::ExtVal || unsafe { out.as_ext_val() }.bits() != host.new_io().bits(),
      vicious: ivm.stats.mem_free < ivm.stats.mem_alloc,
    }
  }

  /// Reduce the `nets` capturing stdout and stderr.
  pub fn output(&self, nets: &Nets) -> (RunResult, Vec<u8>) {
    let input: &[u8] = &[];
    let output = SharedWriter::default();
    let result = self.run(nets, || input, output.as_output_fn());

    (result, output.into_inner())
  }

  /// Calls `inspect_fn` with any captured output and current statistics after
  /// each batch of at least `n` interactions.
  pub fn inspect(&self, nets: &Nets, inspect_fn: impl Fn(Stats, Vec<u8>), n: usize) -> RunResult {
    let input: &[u8] = &[];
    let output = SharedWriter::default();
    let mut host = &mut Host::default();
    let heap = self.heap();
    let extrinsics = &mut Extrinsics::default();
    let (mut ivm, root) =
      self.boot(&mut host, &heap, extrinsics, || input, output.as_output_fn(), nets);

    let mut interactions = 0;
    let mut work_fast = Vec::new();
    let mut work_slow = Vec::new();
    while !ivm.is_finished() {
      interactions += if self.breadth_first {
        ivm.normalize_breadth_first_batch(&mut work_fast, &mut work_slow)
      } else {
        ivm.normalize_batch()
      };

      if interactions > n {
        inspect_fn(ivm.stats, output.0.lock().unwrap().drain(..).collect());
        interactions = 0;
      }
    }

    let out = ivm.follow(root);
    inspect_fn(ivm.stats, output.0.lock().unwrap().drain(..).collect());
    RunResult {
      stats: ivm.stats,
      flags: ivm.flags,
      no_io: out.tag() != Tag::ExtVal || unsafe { out.as_ext_val() }.bits() != host.new_io().bits(),
      vicious: ivm.stats.mem_free < ivm.stats.mem_alloc,
    }
  }

  fn boot<'a: 'ivm, 'ivm, 'ext, R: Read, W: Write>(
    &'a self,
    host: &mut &'ivm mut Host<'ivm>,
    heap: &'ivm Heap,
    extrinsics: &'ext mut Extrinsics<'ivm>,
    io_input_fn: impl Copy + Fn() -> R + Send + Sync + 'ivm,
    io_output_fn: impl Copy + Fn() -> W + Send + Sync + 'ivm,
    nets: &Nets,
  ) -> (IVM<'ivm, 'ext>, Port<'ivm>) {
    host.register_default_extrinsics(extrinsics);
    host.register_runtime_extrinsics(extrinsics, &self.args, io_input_fn, io_output_fn);

    host.insert_nets(nets);
    let main = host.get("::").expect("missing main");
    let mut ivm = IVM::new(heap, extrinsics);
    let node = unsafe { ivm.new_node(Tag::Comb, Host::label_to_u16("x", &mut host.comb_labels)) };
    ivm.link_wire(node.1, Port::new_ext_val(host.new_io()));
    ivm.link(Port::new_global(main), node.0);

    (ivm, Port::new_wire(node.2))
  }

  fn heap(&self) -> Box<Heap> {
    match self.heap {
      Some(size) => Heap::with_size(size).expect("heap allocation failed"),
      None => Heap::new(),
    }
  }
}

pub struct RunResult {
  pub stats: Stats,
  pub flags: Flags,
  pub no_io: bool,
  pub vicious: bool,
}

impl RunResult {
  pub fn success(&self) -> bool {
    !self.no_io && !self.vicious && self.flags.success()
  }
}

#[derive(Default)]
struct SharedWriter(Mutex<Vec<u8>>);

impl SharedWriter {
  fn as_output_fn<'a>(&'a self) -> impl Copy + Fn() -> MutexWriter<'a> + Send + Sync {
    || MutexWriter(self.0.lock().unwrap())
  }

  fn into_inner(self) -> Vec<u8> {
    self.0.into_inner().unwrap()
  }
}

struct MutexWriter<'a>(MutexGuard<'a, Vec<u8>>);

impl Write for MutexWriter<'_> {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.0.extend_from_slice(buf);
    Ok(buf.len())
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}
