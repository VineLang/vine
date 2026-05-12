use std::{
  collections::HashMap,
  io::{self, IsTerminal, stderr},
  process::exit,
};

use clap::Args;
use ivm::{
  host::{
    Host, IVM,
    ext::common,
    runner::{CaptureOutput, Runner},
  },
  runtime::{flags::Flags, heap::Heap, stats::Stats},
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
  /// Run the nets forwarding stdin and stdout, exiting if any errors occur.
  pub fn run(&self, table: &mut Table, nets: &HashMap<NameId, FlatNet>, debug_hint: bool) {
    let mut heap = self.heap();
    let mut ivm = IVM::new();
    let mut host = Host::new(&mut ivm);
    let extrinsics = common::all(&self.args, io::stdin, io::stdout);
    let runner = Runner::new(&mut heap, &mut host, extrinsics, table, nets);

    let (mut stats, flags, mut benches) = runner.normalize(self.breadth_first, self.workers, ());
    for stats in benches.iter_mut().map(|(_, stats)| stats).chain([(&mut stats)]) {
      if self.no_perf {
        stats.clear_perf();
      }
    }

    if !flags.success() {
      eprintln!("\n{}", flags.error_message(debug_hint));
    }

    if !self.no_stats {
      if benches.is_empty() {
        eprintln!("{stats}");
      } else {
        let Colors { reset, bold, .. } = colors(&stderr());

        for (name, stats) in &benches {
          let stats = indent(stats.to_string().trim(), 2);
          eprintln!("\n{bold}Stats for {name:?}{reset}:\n{stats}\n");
        }

        let stats = indent(stats.to_string().trim(), 2);
        eprintln!("\n{bold}Stats for overall execution{reset}: \n{stats}\n");
      }
    }

    if !flags.success() {
      exit(1);
    }
  }

  /// Run the nets with empty stdin and capturing any stdout.
  pub fn run_capture(
    &self,
    table: &mut Table,
    nets: &HashMap<NameId, FlatNet>,
  ) -> (Stats, Flags, Vec<u8>) {
    let capture = CaptureOutput::default();
    let mut heap = self.heap();
    let mut ivm = IVM::new();

    let (stats, flags, _) = {
      let mut host = Host::new(&mut ivm);
      let extrinsics = capture.extrinsics(&self.args);
      let runner = Runner::new(&mut heap, &mut host, extrinsics, table, nets);

      runner.normalize(self.breadth_first, self.workers, ())
    };

    (stats, flags, capture.into_output())
  }

  fn heap(&self) -> Box<Heap> {
    match self.heap {
      Some(size) => Heap::with_size(size).expect("heap allocation failed"),
      None => Heap::new(),
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

pub struct Colors {
  pub reset: &'static str,
  pub bold: &'static str,
  pub underline: &'static str,
  pub grey: &'static str,
  pub red: &'static str,
  pub yellow: &'static str,
  pub green: &'static str,
}

pub fn colors(t: &impl IsTerminal) -> Colors {
  if t.is_terminal() {
    Colors {
      reset: "\x1b[0m",
      bold: "\x1b[1m",
      underline: "\x1b[4m",
      grey: "\x1b[2;39m",
      red: "\x1b[91m",
      yellow: "\x1b[33m",
      green: "\x1b[32m",
    }
  } else {
    Colors { reset: "", bold: "", underline: "", grey: "", red: "", yellow: "", green: "" }
  }
}

fn indent(s: &str, level: usize) -> String {
  s.lines()
    .map(|line| format!("{line:>width$}", width = line.len() + level))
    .collect::<Vec<_>>()
    .join("\n")
}
