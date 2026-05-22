use std::{
  collections::HashMap,
  fmt::Display,
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
  runtime::{
    bench::{Bench, Benches},
    flags::Flags,
    heap::Heap,
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
  /// Run the nets forwarding stdin and stdout, exiting if any errors occur.
  pub fn run(&self, table: &mut Table, nets: &HashMap<NameId, FlatNet>, debug_hint: bool) {
    let mut heap = self.heap();
    let mut ivm = IVM::new();
    let mut host = Host::new(&mut ivm);
    let extrinsics = common::all(&self.args, io::stdin, io::stdout);
    let runner = Runner::new(&mut heap, &mut host, extrinsics, table, nets);

    let (mut stats, flags, mut benches) = runner.normalize(self.breadth_first, self.workers, ());
    for stats in benches.values_mut().map(|bench| &mut bench.stats).chain([(&mut stats)]) {
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
        let Colors { bold, reset, .. } = colors(&stderr());
        print_benches_tree(stats, &benches);
        eprintln!("\nStats for {bold}Overall Execution{reset}:{stats}");
        for Bench { name, stats, .. } in benches.values() {
          eprintln!("\nStats for {bold}{name}{reset}:{}", indent(stats, 2));
        }
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

fn indent(s: impl Display, level: usize) -> String {
  s.to_string().lines().map(|line| format!("{:>level$}{line}", "")).collect::<Vec<_>>().join("\n")
}

fn print_benches_tree(stats: Stats, benches: &Benches) {
  fn _print_benches_tree(
    bench: &Bench,
    benches: &Benches,
    prefix: &mut String,
    is_last: bool,
    lines: &mut Vec<(String, u64, u64)>,
  ) {
    let Bench { name, children, stats, .. } = bench;
    let connector = if is_last { "└── " } else { "├── " };
    lines.push((
      format!("{prefix}{connector}{name}"),
      stats.interactions(),
      stats.time_clock.as_millis() as u64,
    ));
    let cont = if is_last { "    " } else { "│   " };
    prefix.push_str(cont);
    for (i, &id) in children.iter().enumerate() {
      _print_benches_tree(&benches[id], benches, prefix, i == children.len() - 1, lines);
    }
    prefix.truncate(prefix.len() - cont.len());
  }

  fn len_base10(n: u64) -> usize {
    match n {
      0 => 1,
      n => ((n as f64).log10().floor() as usize) + 1,
    }
  }

  let mut lines = vec![(
    "Overall Execution".to_owned(),
    stats.interactions(),
    stats.time_clock.as_millis() as u64,
  )];
  let mut prefix = String::new();
  let root_benches: Vec<_> = benches.values().filter(|b| b.parent.is_none()).collect();
  for (i, bench) in root_benches.iter().enumerate() {
    let is_last = i == root_benches.len() - 1;
    _print_benches_tree(bench, benches, &mut prefix, is_last, &mut lines);
  }

  let mut max_len = 0;
  let mut max_ixs = 0;
  let mut max_ms = 0;
  for (line, ixs, ms) in &lines {
    max_len = max_len.max(line.chars().count());
    max_ixs = max_ixs.max(len_base10(*ixs));
    max_ms = max_ms.max(len_base10(*ms));
  }

  eprintln!();
  for (line, ixs, ms) in lines {
    eprint!("{line:<max_len$}  {ixs:>max_ixs$} ixs");
    if !stats.time_clock.is_zero() {
      eprint!("  {ms:>max_ms$} ms")
    }
    eprintln!();
  }
}
