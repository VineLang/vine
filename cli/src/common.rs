use clap::Args;

use ivm::{ext::Extrinsics, heap::Heap, IVM};
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
}

impl RunArgs {
  pub fn run(self, nets: Nets) {
    let mut host = &mut Host::default();
    let heap = Heap::new();
    let mut extrinsics = Extrinsics::default();

    host.register_default_extrinsics(&mut extrinsics);
    host.insert_nets(&nets);
    let main = host.get("::main").expect("missing main");
    let mut ivm = IVM::new(&heap, &extrinsics);
    ivm.boot(main, host.new_io());
    if self.workers > 0 {
      ivm.normalize_parallel(self.workers)
    } else {
      ivm.normalize();
    }
    if !self.no_stats {
      eprintln!("{}", ivm.stats);
    }
  }
}
