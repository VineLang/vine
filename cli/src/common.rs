use std::{
  collections::HashMap,
  io::{self, Read, Write},
  process::exit,
  sync::{Mutex, MutexGuard},
};

use clap::Args;
use ivm::{
  host::{Host, IVM, ext::common::IO},
  program::Program,
  runtime::{
    flags::Flags,
    heap::Heap,
    port::{Port, Tag},
    stats::Stats,
  },
};
use ivy::{
  name::{NameId, Table},
  net::FlatNet,
};

#[derive(Debug, Args)]
pub struct RunArgs {
  #[arg(long)]
  no_stats: bool,
  #[arg(long)]
  no_perf: bool,
  #[arg(long, short, default_value = "0")]
  workers: usize,
  #[arg(long, alias = "depth", short = 'd')]
  breadth_first: bool,
  #[arg(long, short = 'H', value_parser = parse_size)]
  heap: Option<usize>,
  pub args: Vec<String>,
}

impl RunArgs {
  /// Run the nets forwarding stdout and stderr, exiting if any errors occur.
  pub fn run(&self, table: &mut Table, nets: &HashMap<NameId, FlatNet>, debug_hint: bool) {
    self.run_with(table, nets, io::stdin, io::stdout).report(debug_hint);
  }

  /// Run the nets capturing any stdout and stderr.
  pub fn run_capture(
    &self,
    table: &mut Table,
    nets: &HashMap<NameId, FlatNet>,
  ) -> (RunResult, Vec<u8>) {
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
    let result = self.run_with(table, nets, || input, || SharedWriter(output.lock().unwrap()));

    (result, output.into_inner().unwrap())
  }

  pub fn run_with<R: Read, W: Write>(
    &self,
    table: &mut Table,
    nets: &HashMap<NameId, FlatNet>,
    stdin: impl Copy + Fn() -> R + Send + Sync,
    stdout: impl Copy + Fn() -> W + Send + Sync,
  ) -> RunResult {
    let mut heap = match self.heap {
      Some(size) => Heap::with_size(size).expect("heap allocation failed"),
      None => Heap::new(),
    };

    let mut ivm = IVM::new();
    let mut host = Host::new(&mut ivm);

    let io = host.register_ext_ty::<IO>();

    use ivm::host::ext::common;
    host.register(
      table,
      (
        common::fundamental(),
        common::arithmetic(),
        common::io_meta(),
        common::io_stdio_with(stdin, stdout),
        common::io_args(&self.args),
        common::io_error(),
        common::io_file(),
      ),
    );

    let program = Program::new(&host, table, nets);
    let main = table.add_path_name("iv:main");

    let mut runtime = host.init(&mut heap);

    let main = program.graft(main).expect("missing main");
    let node = unsafe { runtime.new_node(Tag::Comb, 0) };
    runtime.link_wire(node.1, Port::new_ext_val(io.wrap_static(IO)));
    runtime.link(Port::new_graft(main), node.0);

    if self.breadth_first {
      runtime.normalize_breadth_first();
    } else if self.workers > 0 {
      runtime.normalize_parallel(self.workers)
    } else {
      runtime.normalize();
    }

    let out = runtime.follow(Port::new_wire(node.2));
    if self.no_perf {
      runtime.stats.clear_perf();
    }

    RunResult {
      stats: (!self.no_stats).then_some(runtime.stats),
      flags: runtime.flags,
      no_io: out.tag() != Tag::ExtVal || unsafe { out.as_ext_val() }.ty_id() != io.id(),
      vicious: runtime.stats.mem_free < runtime.stats.mem_alloc,
    }
  }
}

pub struct RunResult {
  pub stats: Option<Stats>,
  pub flags: Flags,
  pub no_io: bool,
  pub vicious: bool,
}

impl RunResult {
  pub fn report(&self, debug_hint: bool) {
    if self.no_io {
      eprintln!("\nError: the net did not return its `IO` handle");
      if debug_hint {
        eprintln!("  hint: try running the program in `--debug` mode to see error messages");
      }
    }
    if self.vicious {
      eprintln!("\nError: the net created a vicious circle");
    }
    if self.flags.ext_copy {
      eprintln!("\nError: a linear extrinsic was copied");
    }
    if self.flags.ext_erase {
      eprintln!("\nError: a linear extrinsic was erased");
    }
    if self.flags.ext_generic {
      eprintln!("\nError: an extrinsic function encountered an unspecified error");
    }

    if let Some(stats) = &self.stats {
      eprintln!("{}", stats);
    }

    if !self.success() {
      exit(1);
    }
  }

  pub fn success(&self) -> bool {
    !self.no_io && !self.vicious && self.flags.success()
  }
}

fn parse_size(size: &str) -> anyhow::Result<usize> {
  if let Some(size) = size.strip_suffix("K") {
    Ok(size.parse::<usize>()? << 10)
  } else if let Some(size) = size.strip_suffix("M") {
    Ok(size.parse::<usize>()? << 20)
  } else if let Some(size) = size.strip_suffix("G") {
    Ok(size.parse::<usize>()? << 30)
  } else {
    Ok(size.parse::<usize>()?)
  }
}
