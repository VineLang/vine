use std::{
  fs,
  io::{stdin, Read},
  path::PathBuf,
  process::exit,
};

use anyhow::Result;
use clap::{Args, Parser};

use ivm::{ext::Extrinsics, heap::Heap, IVM};
use ivy::{ast::Nets, host::Host};
use rustyline::DefaultEditor;
use vine::{
  compiler::Compiler,
  core::{Core, CoreArenas},
  repl::Repl,
};
use vine_lsp::lsp;

use super::{Optimizations, RunArgs};

#[derive(Debug, Parser)]
#[command(name = "vine", version, about = "Vine's CLI", propagate_version = true)]
pub enum VineCommand {
  #[command(about = "Build and run a Vine program")]
  Run(VineRunCommand),
  #[command(about = "Compile a Vine program to Ivy")]
  Build(VineBuildCommand),

  Fmt(VineFmtCommand),
  Repl(VineReplCommand),
  Lsp(VineLspCommand),
}

impl VineCommand {
  pub fn execute() -> Result<()> {
    match Self::parse() {
      VineCommand::Run(run) => run.execute(),
      VineCommand::Build(build) => build.execute(),
      VineCommand::Repl(repl) => repl.execute(),
      VineCommand::Fmt(fmt) => fmt.execute(),
      VineCommand::Lsp(lsp) => lsp.execute(),
    }
  }
}

#[derive(Debug, Args)]
pub struct CompileArgs {
  #[arg()]
  main: Option<PathBuf>,
  #[arg(long = "lib")]
  libs: Vec<PathBuf>,
  #[arg(long)]
  no_std: bool,
}

impl CompileArgs {
  fn compile(mut self) -> Nets {
    if !self.no_std {
      self.libs.push(std_path())
    }

    let arenas = CoreArenas::default();
    let core = &Core::new(&arenas);
    let mut compiler = Compiler::new(core);

    if let Some(main) = self.main {
      compiler.loader.load_main_mod(main);
    }
    for lib in self.libs {
      compiler.loader.load_mod(lib);
    }

    match compiler.compile(()) {
      Ok(nets) => nets,
      Err(err) => {
        eprintln!("{}", err);
        exit(1);
      }
    }
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
  #[arg(long)]
  echo: bool,
}

impl VineReplCommand {
  pub fn execute(mut self) -> Result<()> {
    if !self.no_std {
      self.libs.push(std_path())
    }

    let host = &mut Host::default();
    let heap = Heap::new();
    let mut extrinsics = Extrinsics::default();
    host.register_default_extrinsics(&mut extrinsics);

    let mut ivm = IVM::new(&heap, &extrinsics);
    let arenas = CoreArenas::default();
    let core = &Core::new(&arenas);
    let mut repl = match Repl::new(host, &mut ivm, core, self.libs) {
      Ok(repl) => repl,
      Err(err) => {
        eprintln!("{err}");
        exit(1);
      }
    };
    let mut rl = DefaultEditor::new()?;
    loop {
      print!("\n{repl}");
      match rl.readline("> ") {
        Ok(line) => {
          if self.echo {
            println!("> {}", line);
          }
          _ = rl.add_history_entry(&line);
          match repl.exec(&line) {
            Ok(Some(result)) => println!("{result}"),
            Ok(None) => {}
            Err(err) => println!("{err}"),
          }
        }
        Err(_) => break,
      }
    }
    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct VineFmtCommand {}

impl VineFmtCommand {
  pub fn execute(self) -> Result<()> {
    let mut src = String::new();
    stdin().read_to_string(&mut src)?;
    let arenas = CoreArenas::default();
    let core = &Core::new(&arenas);
    println!("{}", core.fmt(&src).unwrap());
    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct VineLspCommand {
  entrypoints: Vec<String>,
  #[arg(long)]
  no_std: bool,
}

impl VineLspCommand {
  pub fn execute(mut self) -> Result<()> {
    if !self.no_std {
      self.entrypoints.push(std_path().display().to_string());
    }
    lsp(self.entrypoints);
    Ok(())
  }
}
