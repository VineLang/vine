use std::{fs, path::PathBuf};

use anyhow::Result;
use clap::{Args, Parser};

use ivy::ast::Nets;

use super::{Optimizations, RunArgs};

#[derive(Debug, Parser)]
#[command(name = "vine", version, about = "Vine's CLI", propagate_version = true)]
pub enum VineCommand {
  #[command(about = "Build and run a Vine program")]
  Run(VineRunCommand),
  #[command(about = "Compile a Vine program to Ivy")]
  Build(VineBuildCommand),
}

impl VineCommand {
  pub fn execute() -> Result<()> {
    match Self::parse() {
      VineCommand::Run(run) => run.execute(),
      VineCommand::Build(build) => build.execute(),
    }
  }
}

#[derive(Debug, Args)]
pub struct CompileArgs {
  #[arg()]
  main: Option<PathBuf>,
  #[arg(long = "lib")]
  libs: Vec<PathBuf>,
  #[arg(long = "item")]
  items: Vec<String>,
  #[arg(long)]
  no_std: bool,
}

impl CompileArgs {
  fn compile(mut self) -> Nets {
    if !self.no_std {
      self.libs.push(std_path())
    }
    vine::compiler(vine::Config { main: self.main, libs: self.libs, items: self.items })
  }
}

fn std_path() -> PathBuf {
  let mut path = PathBuf::new();
  path.push(env!("CARGO_MANIFEST_DIR"));
  path.push("../vine/std/std.vi");
  path
}

#[derive(Debug, Args)]
pub struct VineRunCommand {
  #[command(flatten)]
  compile: CompileArgs,
  #[command(flatten)]
  run_args: RunArgs,
}

impl VineRunCommand {
  pub fn execute(self) -> Result<()> {
    if self.compile.main.is_none() {
      panic!("must supply main")
    }
    let mut nets = self.compile.compile();
    Optimizations::default().apply(&mut nets);
    self.run_args.run(nets);
    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct VineBuildCommand {
  #[command(flatten)]
  compile: CompileArgs,
  #[command(flatten)]
  optimizations: Optimizations,
  #[arg(long)]
  out: Option<PathBuf>,
}

impl VineBuildCommand {
  pub fn execute(self) -> Result<()> {
    let mut nets = self.compile.compile();
    self.optimizations.apply(&mut nets);
    if let Some(out) = self.out {
      fs::write(out, nets.to_string())?;
    } else {
      println!("{nets}");
    }
    Ok(())
  }
}
