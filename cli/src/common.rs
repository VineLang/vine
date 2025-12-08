use std::{
  io::{self, Read, Write},
  process::exit,
  sync::{Arc, Mutex},
};

use clap::Args;

use ivm::{
  IVM,
  ext::Extrinsics,
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
  pub fn apply(self, nets: &mut Nets) {
    if !self.no_opt {
      Optimizer::default().optimize(nets)
    }
  }
}

#[derive(Debug, Args)]
pub struct RunArgs {
  #[arg(long)]
  no_stats: bool,
  #[arg(long, short, default_value = "0")]
  workers: usize,
  #[arg(long, alias = "depth", short = 'd')]
  breadth_first: bool,
  #[arg(long, short = 'H', value_parser = parse_size)]
  heap: Option<usize>,
}

impl RunArgs {
  pub fn run(&self, nets: &Nets) -> RunResult {
    self._run(nets, io::stdin, io::stdout)
  }

  /// Runs the `nets` with an empty stdin and capturing any writes to stdout.
  pub fn run_capturing_output(&self, nets: &Nets) -> RunResult {
    #[derive(Clone, Default)]
    pub struct SharedWriter(pub Arc<Mutex<Vec<u8>>>);

    impl Write for SharedWriter {
      fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut v = self.0.lock().unwrap();
        v.extend_from_slice(buf);
        Ok(buf.len())
      }

      fn flush(&mut self) -> io::Result<()> {
        Ok(())
      }
    }

    impl SharedWriter {
      fn clone_vec(&self) -> Vec<u8> {
        self.0.lock().unwrap().clone()
      }
    }

    let input: &[u8] = &[];
    let output = SharedWriter::default();

    let mut result = self._run(nets, || input, || output.clone());
    result.output = Some(output.clone_vec());

    result
  }

  fn _run<R, W, FI, FO>(&self, nets: &Nets, io_input_fn: FI, io_output_fn: FO) -> RunResult
  where
    FI: Fn() -> R + Sync,
    FO: Clone + Fn() -> W + Sync,
    W: Write,
    R: Read,
  {
    let mut host = &mut Host::default();
    let heap = match self.heap {
      Some(size) => Heap::with_size(size).expect("heap allocation failed"),
      None => Heap::new(),
    };
    let mut extrinsics = Extrinsics::default();

    host.register_default_extrinsics(&mut extrinsics, io_input_fn, io_output_fn);
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
      stats: if self.no_stats { None } else { Some(ivm.stats) },
      no_io: out.tag() != Tag::ExtVal || unsafe { out.as_ext_val() }.bits() != host.new_io().bits(),
      vicious: ivm.stats.mem_free < ivm.stats.mem_alloc,
      output: None,
    }
  }
}

pub struct RunResult {
  pub stats: Option<Stats>,
  pub no_io: bool,
  pub vicious: bool,
  pub output: Option<Vec<u8>>,
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

    if let Some(stats) = &self.stats {
      eprintln!("{}", stats);
    }

    if self.no_io || self.vicious {
      exit(1);
    }
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
