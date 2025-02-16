use std::{fs, path::PathBuf};

use anyhow::Result;
use clap::{Args, Parser};
use rustyline::DefaultEditor;

use ivm::{ext::Extrinsics, heap::Heap, IVM};
use ivy::{host::Host, parser::IvyParser, repl::Repl};

use crate::{Optimizations, RunArgs};

#[derive(Debug, Parser)]
#[command(name = "ivy", version, about = "Ivy's CLI", propagate_version = true)]
pub enum IvyCommand {
  #[command(about = "Run an Ivy program")]
  Run(IvyRunCommand),
  #[command(about = "Optimize an Ivy program")]
  Optimize(IvyOptimizeCommand),
  Repl(IvyReplCommand),
}

impl IvyCommand {
  pub fn execute() -> Result<()> {
    match Self::parse() {
      IvyCommand::Run(run) => run.execute(),
      IvyCommand::Optimize(optimize) => optimize.execute(),
      IvyCommand::Repl(repl) => repl.execute(),
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

#[derive(Debug, Args)]
pub struct IvyReplCommand {
  #[arg()]
  src: Option<PathBuf>,
  #[command(flatten)]
  run_args: RunArgs,
}

impl IvyReplCommand {
  pub fn execute(self) -> Result<()> {
    let src = self.src.map(fs::read_to_string).unwrap_or(Ok(String::new()))?;
    let nets = IvyParser::parse(&src).unwrap();

    let mut host = &mut Host::default();
    let heap = Heap::new();
    let mut extrinsics = Extrinsics::default();

    host.register_default_extrinsics(&mut extrinsics);
    host.insert_nets(&nets);

    let mut ivm = IVM::new(&heap, &extrinsics);
    let mut repl = Repl::new(host, &mut ivm);
    let mut rl = DefaultEditor::new()?;
    loop {
      print!("\n{repl}");
      match rl.readline("> ") {
        Ok(line) => {
          _ = rl.add_history_entry(&line);
          if let Err(err) = repl.exec(&line) {
            println!("{err:?}");
          }
        }
        Err(_) => break,
      }
    }
    Ok(())
  }
}
