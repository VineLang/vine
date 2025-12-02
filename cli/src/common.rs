use std::process::exit;

use clap::Args;

use ivm::{
  IVM,
  ext::Extrinsics,
  heap::Heap,
  port::{Port, Tag},
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
  #[arg(long, short)]
  heap: Option<String>,
}

impl RunArgs {
  pub fn run(self, nets: Nets, debug_hint: bool) {
    let mut host = &mut Host::default();
    let heap = match self.heap {
      Some(size) => {
        Heap::with_size(parse_size(&size).expect("invalid size")).expect("heap allocation failed")
      }
      None => Heap::new(),
    };
    let mut extrinsics = Extrinsics::default();

    host.register_default_extrinsics(&mut extrinsics);
    host.insert_nets(&nets);

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
    let no_io =
      out.tag() != Tag::ExtVal || unsafe { out.as_ext_val() }.bits() != host.new_io().bits();
    let vicious = ivm.stats.mem_free < ivm.stats.mem_alloc;
    if no_io {
      eprintln!("\nError: the net did not return its `IO` handle");
      if debug_hint {
        eprintln!("  hint: try running the program in `--debug` mode to see error messages");
      }
    }
    if vicious {
      eprintln!("\nError: the net created a vicious circle");
    }

    if !self.no_stats {
      eprintln!("{}", ivm.stats);
    }

    if no_io || vicious {
      exit(1);
    }
  }
}

fn parse_size(size: &str) -> Option<usize> {
  if let Some(size) = size.strip_suffix("K") {
    Some(size.parse::<usize>().ok()? << 10)
  } else if let Some(size) = size.strip_suffix("M") {
    Some(size.parse::<usize>().ok()? << 20)
  } else if let Some(size) = size.strip_suffix("G") {
    Some(size.parse::<usize>().ok()? << 30)
  } else {
    Some(size.parse::<usize>().ok()?)
  }
}
