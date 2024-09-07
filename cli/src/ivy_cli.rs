use std::fs;
use std::path::PathBuf;

use anyhow::Result;
use clap::{Args, Parser};
use ivy::parser::IvyParser;

use crate::{Optimizations, RunArgs};

#[derive(Debug, Parser)]
#[command(name = "ivy", version, about = "Ivy's CLI", propagate_version = true)]
pub enum IvyCommand {
  #[command(about = "Run an Ivy program")]
  Run(IvyRunCommand),
  #[command(about = "Optimize an Ivy program")]
  Optimize(IvyOptimizeCommand),
}

impl IvyCommand {
  pub fn execute() -> Result<()> {
    match Self::parse() {
      IvyCommand::Run(run) => run.execute(),
      IvyCommand::Optimize(optimize) => optimize.execute(),
    }
  }
}

#[derive(Debug, Args)]
pub struct IvyRunCommand {
  #[arg()]
  src: PathBuf,
  #[command(flatten)]
  run_args: RunArgs,
}

impl IvyRunCommand {
  pub fn execute(self) -> Result<()> {
    let src_contents = fs::read_to_string(self.src.clone())?;
    let nets = IvyParser::parse(&src_contents).unwrap();
    self.run_args.run(nets);
    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct IvyOptimizeCommand {
  #[arg(index = 1, default_value = "main.iv")]
  src: String,
  #[arg(name = "out", long, short = 'o')]
  maybe_out: String,
  #[command(flatten)]
  optimizations: Optimizations,
}

impl IvyOptimizeCommand {
  pub fn execute(&self) -> Result<()> {
    unimplemented!()
  }
}
