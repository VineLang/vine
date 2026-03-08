use std::{
  io::{self},
  process::exit,
};

use clap::Args;

use ivy::{ast::Nets, optimize::Optimizer, run::Run};

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

impl From<RunArgs> for Run {
  fn from(run_args: RunArgs) -> Self {
    Run {
      workers: run_args.workers,
      breadth_first: run_args.breadth_first,
      heap: run_args.heap,
      args: run_args.args,
    }
  }
}

impl RunArgs {
  /// Run the nets forwarding stdout and stderr, exiting if any errors occur.
  pub fn check(self, nets: &Nets, debug_hint: bool) {
    let no_stats = self.no_stats;
    let no_perf = self.no_perf;

    let run = Run::from(self);
    let mut result = run.run(nets, io::stdin, io::stdout);
    if no_perf {
      result.stats.clear_perf();
    }

    if result.no_io {
      eprintln!("\nError: the net did not return its `IO` handle");
      if debug_hint {
        eprintln!("  hint: try running the program in `--debug` mode to see error messages");
      }
    }
    if result.vicious {
      eprintln!("\nError: the net created a vicious circle");
    }
    if result.flags.ext_copy {
      eprintln!("\nError: a linear extrinsic was copied");
    }
    if result.flags.ext_erase {
      eprintln!("\nError: a linear extrinsic was erased");
    }
    if result.flags.ext_generic {
      eprintln!("\nError: an extrinsic function encountered an unspecified error");
    }

    if !no_stats {
      eprintln!("{}", result.stats);
    }

    if !result.success() {
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
