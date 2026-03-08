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
  /// Reduce the `nets` capturing stdout and stderr.
  pub fn output(&self, nets: &Nets) -> (RunResult, Vec<u8>) {
    pub struct SharedWriter<'a>(pub MutexGuard<'a, Vec<u8>>);

    impl Write for SharedWriter<'_> {
      fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.extend_from_slice(buf);
        Ok(buf.len())
      }

      fn flush(&mut self) -> io::Result<()> {
        Ok(())
      }
    }

    let input: &[u8] = &[];
    let output = Mutex::default();
    let result = self.run(nets, || input, || SharedWriter(output.lock().unwrap()));

    (result, output.into_inner().unwrap())
  }

  pub fn run<R: Read, W: Write>(
    &self,
    nets: &Nets,
    io_input_fn: impl Copy + Fn() -> R + Send + Sync,
    io_output_fn: impl Copy + Fn() -> W + Send + Sync,
  ) -> RunResult {
    let mut host = &mut Host::default();
    let heap = match self.heap {
      Some(size) => Heap::with_size(size).expect("heap allocation failed"),
      None => Heap::new(),
    };
    let mut extrinsics = Extrinsics::default();

    host.register_default_extrinsics(&mut extrinsics);
    host.register_runtime_extrinsics(&mut extrinsics, &self.args, io_input_fn, io_output_fn);
    host.insert_nets(nets);

    let main = host.get("::").expect("missing main");
    let mut ivm = IVM::new(&heap, &extrinsics);
    let node = unsafe { ivm.new_node(Tag::Comb, Host::label_to_u16("x", &mut host.comb_labels)) };
    ivm.link_wire(node.1, Port::new_ext_val(host.new_io()));
    ivm.link(Port::new_global(main), node.0);

    if self.breadth_first {
      ivm.normalize_breadth_first();
    } else if self.workers > 0 {
      ivm.normalize_parallel(self.workers)
    } else {
      ivm.normalize();
    }

    let out = ivm.follow(Port::new_wire(node.2));
    RunResult {
      stats: ivm.stats,
      flags: ivm.flags,
      no_io: out.tag() != Tag::ExtVal || unsafe { out.as_ext_val() }.bits() != host.new_io().bits(),
      vicious: ivm.stats.mem_free < ivm.stats.mem_alloc,
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
