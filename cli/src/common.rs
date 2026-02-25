use std::{
  io::{self, Read, Write},
  process::exit,
  sync::{Mutex, MutexGuard},
};

use clap::Args;

use ivm::{
  IVM,
  ext::Extrinsics,
  flags::Flags,
  heap::Heap,
  port::{Port, Tag},
  stats::Stats,
};
use ivy::{ast::Nets, host::Host, optimize::Optimizer};

#[derive(Debug, Default, Args)]
pub struct Optimizations {
  #[arg(long)]
  no_opt: bool,
}

impl Optimizations {
  pub fn apply(&self, nets: &mut Nets, entrypoints: &[String]) {
    if !self.no_opt {
      Optimizer::default().optimize(nets, entrypoints)
    }
  }
}

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
  /// Run the nets capturing any stdout and stderr.
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

  /// Run the nets forwarding stdout and stderr, exiting if any errors occur.
  pub fn check(&self, nets: &Nets, debug_hint: bool) {
    self.run(nets, io::stdin, io::stdout).check(debug_hint);
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
    if self.no_perf {
      ivm.stats.clear_perf();
    }

    RunResult {
      stats: (!self.no_stats).then_some(ivm.stats),
      flags: ivm.flags,
      no_io: out.tag() != Tag::ExtVal || unsafe { out.as_ext_val() }.bits() != host.new_io().bits(),
      vicious: ivm.stats.mem_free < ivm.stats.mem_alloc,
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
  pub fn check(&self, debug_hint: bool) {
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
