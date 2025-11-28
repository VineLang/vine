use std::{
  env, fs,
  io::{self, Read, stdin},
  path::PathBuf,
  process::exit,
};

use anyhow::Result;
use clap::{Args, CommandFactory, Parser};

use ivm::{IVM, ext::Extrinsics, heap::Heap};
use ivy::{ast::Nets, host::Host};
use rustyline::DefaultEditor;
use vine::{
  compiler::Compiler,
  features::cfg::Config,
  tools::{fmt::Formatter, repl::Repl},
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

  #[command(about = "Generate shell completion scripts")]
  Completion(VineCompletionCommand),
}

impl VineCommand {
  pub fn execute() -> Result<()> {
    match Self::parse() {
      VineCommand::Run(run) => run.execute(),
      VineCommand::Build(build) => build.execute(),
      VineCommand::Repl(repl) => repl.execute(),
      VineCommand::Fmt(fmt) => fmt.execute(),
      VineCommand::Lsp(lsp) => lsp.execute(),
      VineCommand::Completion(completion) => completion.execute(),
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
  no_root: bool,
  #[arg(long)]
  debug: bool,
}

impl CompileArgs {
  fn compile(mut self) -> Nets {
    if !self.no_root {
      self.libs.push(root_path())
    }

    let mut compiler = Compiler::new(self.debug, Config::default());

    if let Some(main) = self.main {
      compiler.loader.load_main_mod(&main, &mut compiler.diags);
    }
    for lib in self.libs {
      compiler.loader.load_mod(&lib, &mut compiler.diags);
    }

    match compiler.compile(()) {
      Ok(nets) => nets,
      Err(diags) => {
        eprintln!("{}", compiler.loader.print_diags(&diags));
        exit(1);
      }
    }
  }
}

fn root_path() -> PathBuf {
  let mut path = PathBuf::new();
  let compile_time_root_path = option_env!("VINE_ROOT_PATH");
  let runtime_root_path = env::var("VINE_ROOT_PATH").ok();

  match runtime_root_path.as_deref().or(compile_time_root_path) {
    Some(root_path) => {
      path.push(root_path);
      path.push("root.vi");
    }
    None => {
      path.push(env!("CARGO_MANIFEST_DIR"));
      path.push("../root/root.vi");
    }
  }
  path
}

#[derive(Debug, Args)]
pub struct VineRunCommand {
  #[command(flatten)]
  compile: CompileArgs,
  #[command(flatten)]
  optimizations: Optimizations,
  #[command(flatten)]
  run_args: RunArgs,
}

impl VineRunCommand {
  pub fn execute(self) -> Result<()> {
    if self.compile.main.is_none() {
      panic!("must supply main")
    }
    let debug = self.compile.debug;
    let mut nets = self.compile.compile();
    self.optimizations.apply(&mut nets);
    self.run_args.run(nets, !debug);
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
  no_root: bool,
  #[arg(long)]
  echo: bool,
  #[arg(long)]
  no_debug: bool,
}

impl VineReplCommand {
  pub fn execute(mut self) -> Result<()> {
    if !self.no_root {
      self.libs.push(root_path())
    }

    let host = &mut Host::default();
    let heap = Heap::new();
    let mut extrinsics = Extrinsics::default();
    host.register_default_extrinsics(&mut extrinsics);

    let mut ivm = IVM::new(&heap, &extrinsics);
    let mut compiler = Compiler::new(!self.no_debug, Config::default());
    let mut repl = match Repl::new(host, &mut ivm, &mut compiler, self.libs) {
      Ok(repl) => repl,
      Err(diags) => {
        eprintln!("{}", compiler.loader.print_diags(&diags));
        exit(1);
      }
    };
    let mut rl = DefaultEditor::new()?;
    loop {
      println!();
      if repl.options.show_scope {
        repl.print_scope();
      }
      match rl.readline("> ") {
        Ok(line) => {
          if self.echo {
            println!("> {line}");
          }
          _ = rl.add_history_entry(&line);
          if let Err(diags) = repl.exec(&line) {
            println!("{}", repl.print_diags(&diags))
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
    println!("{}", Formatter::fmt(&src).unwrap());
    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct VineLspCommand {
  #[arg(long)]
  libs: Vec<PathBuf>,
  entrypoints: Vec<String>,
  #[arg(long)]
  no_root: bool,
}

impl VineLspCommand {
  pub fn execute(mut self) -> Result<()> {
    if !self.no_root {
      self.libs.push(root_path());
    };
    if let Err(err) = lsp(self.libs, self.entrypoints) {
      eprintln!("{err}");
      exit(1);
    }
    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct VineCompletionCommand {
  pub shell: clap_complete::Shell,
}

impl VineCompletionCommand {
  pub fn execute(self) -> Result<()> {
    let shell = self.shell;

    let mut cmd = VineCommand::command();
    let cmd_name = cmd.get_name().to_string();
    clap_complete::generate(shell, &mut cmd, cmd_name, &mut io::stdout());

    Ok(())
  }
}
