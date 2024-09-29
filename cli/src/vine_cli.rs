use std::{fs, path::PathBuf};

use anyhow::Result;
use clap::{Args, Parser};

use ivm::{heap::Heap, IVM};
use ivy::{ast::Nets, host::Host};
use rustyline::DefaultEditor;
use vine::repl::Repl;
use vine_util::{arena::BytesArena, interner::StringInterner};

use super::{Optimizations, RunArgs};

#[derive(Debug, Parser)]
#[command(name = "vine", version, about = "Vine's CLI", propagate_version = true)]
pub enum VineCommand {
  #[command(about = "Build and run a Vine program")]
  Run(VineRunCommand),
  #[command(about = "Compile a Vine program to Ivy")]
  Build(VineBuildCommand),

  Repl(VineReplCommand),
}

impl VineCommand {
  pub fn execute() -> Result<()> {
    match Self::parse() {
      VineCommand::Run(run) => run.execute(),
      VineCommand::Build(build) => build.execute(),
      VineCommand::Repl(repl) => repl.execute(),
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
    vine::build(vine::Config { main: self.main, libs: self.libs, items: self.items })
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

#[derive(Debug, Args)]
pub struct VineReplCommand {
  #[arg(long)]
  libs: Vec<PathBuf>,
  #[arg(long)]
  no_std: bool,
}

impl VineReplCommand {
  pub fn execute(mut self) -> Result<()> {
    if !self.no_std {
      self.libs.push(std_path())
    }

    let host = &mut Host::default();
    let heap = Heap::new();
    let mut ivm = IVM::new(&heap);
    let arena = &*Box::leak(Box::new(BytesArena::default()));
    let interner = StringInterner::new(arena);
    let mut repl = Repl::new(host, &mut ivm, &interner, self.libs);
    let mut rl = DefaultEditor::new()?;
    loop {
      print!("\n{repl}");
      match rl.readline("> ") {
        Ok(line) => {
          _ = rl.add_history_entry(&line);
          match repl.exec(&line) {
            Ok(Some(result)) => println!("{result}"),
            Ok(None) => {}
            Err(err) => println!("{err:?}"),
          }
        }
        Err(_) => break,
      }
    }
    Ok(())
  }
}
