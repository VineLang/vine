use std::{
  collections::HashMap,
  io::{self, Write},
  sync::{Mutex, MutexGuard},
};

use ivy::{
  name::{NameId, Table},
  net::FlatNet,
};
use vine_util::register::Register;

use crate::{
  host::{
    Host,
    ext::common::{self, IO},
  },
  program::Program,
  runtime::{
    Hooks, Runtime,
    bench::Benches,
    ext::ExtTy,
    flags::Flags,
    heap::Heap,
    port::{Port, Tag},
    stats::Stats,
    wire::Wire,
  },
};

pub struct Runner<'ivm, 'ext> {
  io: ExtTy<'ivm, IO>,
  runtime: Runtime<'ivm, 'ext>,
  root: Wire<'ivm>,
}

impl<'ivm, 'ext> Runner<'ivm, 'ext> {
  pub fn new(
    heap: &'ivm mut Heap,
    host: &'ext mut Host<'ivm>,
    extrinsics: impl Register<Host<'ivm>>,
    table: &mut Table,
    nets: &HashMap<NameId, FlatNet>,
  ) -> Self {
    let io = host.register_ext_ty::<IO>();

    host.register(table, extrinsics);

    let program = Program::new(host, table, nets);
    let main = table.add_path_name("iv:main");
    let main = program.graft(main).expect("missing main");

    let mut runtime = host.init(heap);

    let node = unsafe { runtime.new_node(Tag::Comb, 0) };
    runtime.link_wire(node.1, Port::new_ext_val(io.wrap_static(IO)));
    runtime.link(Port::new_graft(main), node.0);

    Self { io, root: node.2, runtime }
  }

  pub fn normalize(
    mut self,
    breadth_first: bool,
    workers: usize,
    hooks: impl Hooks,
  ) -> (Stats, Flags, Benches) {
    if breadth_first {
      self.runtime.normalize_breadth_first(hooks);
    } else if workers > 0 {
      self.runtime.normalize_parallel(workers, ())
    } else {
      self.runtime.normalize(hooks);
    }

    let out = self.runtime.follow(Port::new_wire(self.root));

    self.runtime.flags.no_io =
      out.tag() != Tag::ExtVal || unsafe { out.as_ext_val() }.ty_id() != self.io.id();
    self.runtime.flags.vicious = self.runtime.stats.mem_free < self.runtime.stats.mem_alloc;

    (self.runtime.stats, self.runtime.flags, self.runtime.bencher.benches)
  }
}

#[derive(Default)]
pub struct CaptureOutput {
  pub output: Mutex<Vec<u8>>,
}

impl CaptureOutput {
  pub fn extrinsics<'a: 'ivm, 'b: 'ivm, 'ivm>(
    &'a self,
    args: &'b [String],
  ) -> impl Register<Host<'ivm>> where {
    common::all(args, || &[][..], || SharedWriter(self.output.lock().unwrap()))
  }

  pub fn into_output(self) -> Vec<u8> {
    self.output.into_inner().unwrap()
  }
}

struct SharedWriter<'a>(MutexGuard<'a, Vec<u8>>);

impl Write for SharedWriter<'_> {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    self.0.extend_from_slice(buf);
    Ok(buf.len())
  }

  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}
