use std::{
  env, fs,
  io::{self, IsTerminal, Read, stderr, stdin, stdout},
  path::PathBuf,
  process::exit,
  sync::mpsc,
  time::{Duration, Instant},
};

use anyhow::Result;
use clap::{Args, CommandFactory, Parser};

use ivm::{IVM, ext::Extrinsics, heap::Heap};
use ivy::{ast::Nets, host::Host};
use notify::{RecommendedWatcher, RecursiveMode, Watcher};
use rustyline::DefaultEditor;
use vine::{
  compiler::Compiler,
  features::cfg::Config,
  tools::{doc::document, fmt::Formatter, repl::Repl},
};
use vine_lsp::lsp;

use super::{Optimizations, RunArgs};

#[derive(Debug, Parser)]
#[command(name = "vine", version, about = "Vine's CLI", propagate_version = true)]
pub enum VineCommand {
  /// Build and run a Vine program
  Run(VineRunCommand),
  /// Compile a Vine program to Ivy
  Build(VineBuildCommand),
  Check(VineCheckCommand),
  /// Run tests in a Vine program
  Test(VineTestCommand),

  Fmt(VineFmtCommand),
  Repl(VineReplCommand),
  Lsp(VineLspCommand),

  /// Generate shell completion scripts
  Completion(VineCompletionCommand),

  #[command(hide = true)]
  Doc(VineDocCommand),
}

impl VineCommand {
  pub fn execute() -> Result<()> {
    match Self::parse() {
      VineCommand::Run(run) => run.execute(),
      VineCommand::Build(build) => build.execute(),
      VineCommand::Check(check) => check.execute(),
      VineCommand::Test(test) => test.execute(),
      VineCommand::Repl(repl) => repl.execute(),
      VineCommand::Fmt(fmt) => fmt.execute(),
      VineCommand::Lsp(lsp) => lsp.execute(),
      VineCommand::Completion(completion) => completion.execute(),
      VineCommand::Doc(doc) => doc.execute(),
    }
  }
}

#[derive(Debug, Args)]
pub struct CheckArgs {
  entrypoints: Vec<String>,
  #[arg(long = "lib")]
  libs: Vec<PathBuf>,
  #[arg(long)]
  no_root: bool,
}

#[derive(Debug, Args)]
pub struct CompileArgs {
  main: PathBuf,
  #[arg(long = "lib")]
  libs: Vec<PathBuf>,
  #[arg(long)]
  no_root: bool,
  #[arg(long)]
  debug: bool,
  #[arg(hide = true, long, default_value_t)]
  test: bool,
}

impl CompileArgs {
  fn initialize(mut self) -> Compiler {
    if !self.no_root {
      self.libs.push(root_path())
    }

    let config = Config::new(self.debug, self.test);
    let mut compiler = Compiler::new(config);

    compiler.loader.load_main_mod(&self.main, &mut compiler.diags);
    for lib in self.libs {
      compiler.loader.load_mod(&lib, &mut compiler.diags);
    }

    compiler
  }

  fn compile(self) -> (Nets, Compiler) {
    let mut compiler = self.initialize();
    let result = compiler.compile(());
    eprint_diags(&compiler);
    match result {
      Ok(nets) => (nets, compiler),
      Err(_) => {
        exit(1);
      }
    }
  }
}

fn root_path() -> PathBuf {
  let mut path = env::current_exe().unwrap();
  path.pop();
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
    let debug = self.compile.debug;
    let (mut nets, _) = self.compile.compile();
    self.optimizations.apply(&mut nets);
    self.run_args.run(nets, !debug);
    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct VineTestCommand {
  #[command(flatten)]
  compile: CompileArgs,
  #[command(flatten)]
  optimizations: Optimizations,
  #[command(flatten)]
  run_args: RunArgs,

  /// The test path to run. If not provided, all test paths in MAIN are printed.
  test_path: Option<String>,
}

impl VineTestCommand {
  pub fn execute(mut self) -> Result<()> {
    let debug = self.compile.debug;
    self.compile.test = true;

    let (mut nets, mut compiler) = self.compile.compile();

    let Some(test_path) = self.test_path else {
      for concrete_fn_id in compiler.chart.tests {
        let def_id = compiler.chart.concrete_fns[concrete_fn_id].def;
        let path = &compiler.chart.defs[def_id].path;

        println!("{path}");
      }

      return Ok(());
    };

    let Some(test_concrete_fn_id) = compiler.chart.tests.iter().find(|concrete_fn_id| {
      let def_id = compiler.chart.concrete_fns[**concrete_fn_id].def;
      compiler.chart.defs[def_id].path == test_path
    }) else {
      eprintln!("test path {test_path:?} does not exist");
      exit(1);
    };

    let main_fragment_id = compiler.resolutions.fns[*test_concrete_fn_id];
    compiler.insert_main_net(&mut nets, main_fragment_id);

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
    let (mut nets, _) = self.compile.compile();
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
pub struct VineCheckCommand {
  #[command(flatten)]
  check: CheckArgs,
  #[arg(long)]
  watch: Vec<PathBuf>,
  #[arg(long, default_value = "50")]
  debounce: u64,
}

impl VineCheckCommand {
  pub fn execute(mut self) -> Result<()> {
    if !self.check.no_root {
      self.check.libs.push(root_path())
    }

    let config = Config::new(true, true);
    let mut compiler = Compiler::new(config);

    for lib in self.check.libs {
      compiler.loader.load_mod(&lib, &mut compiler.diags);
    }

    if compiler.check(()).is_err() {
      eprint_diags(&compiler);
      exit(1);
    }

    let checkpoint = compiler.checkpoint();

    let Colors { reset, red, yellow, green, grey, .. } = colors(&stderr());

    let mut check = move || {
      let start = Instant::now();

      compiler.revert(&checkpoint);

      for glob in &self.check.entrypoints {
        for entry in glob::glob(glob).unwrap() {
          let entry = entry.unwrap();
          compiler.loader.load_mod(&entry, &mut compiler.diags);
        }
      }

      _ = compiler.check(());

      eprint_diags(&compiler);

      let color = if !compiler.diags.errors.is_empty() {
        red
      } else if !compiler.diags.warnings.is_empty() {
        yellow
      } else {
        green
      };

      eprintln!("{color}finished{reset} {grey}in{reset} {:?}", start.elapsed());
    };

    let clear = || {
      eprint!("\x1b[H\x1b[3J\x1b[2J");
    };

    if self.watch.is_empty() {
      check();
    } else {
      let (send, recv) = mpsc::channel();
      let mut watcher = RecommendedWatcher::new(send, Default::default())?;

      for path in self.watch {
        watcher.watch(&path, RecursiveMode::Recursive)?;
      }

      let debounce = Duration::from_millis(self.debounce);
      loop {
        clear();
        check();
        _ = recv.recv();
        while recv.recv_timeout(debounce).is_ok() {}
      }
    }

    Ok(())
  }
}

#[derive(Debug, Args)]
pub struct VineReplCommand {
  #[arg(long = "lib")]
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
    let config = Config::new(!self.no_debug, false);
    let mut compiler = Compiler::new(config);
    let mut repl = match Repl::new(host, &mut ivm, &mut compiler, self.libs) {
      Ok(repl) => repl,
      Err(_) => {
        eprint_diags(&compiler);
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
          let checkpoint = repl.compiler.checkpoint();
          if repl.exec(&line).is_err() {
            repl.compiler.diags.warnings.clear();
            print_diags(repl.compiler);
            repl.compiler.revert(&checkpoint);
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
  #[command(flatten)]
  check: CheckArgs,
}

impl VineLspCommand {
  pub fn execute(mut self) -> Result<()> {
    if !self.check.no_root {
      self.check.libs.push(root_path());
    };
    lsp(self.check.libs, self.check.entrypoints);
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

#[derive(Debug, Args)]
pub struct VineDocCommand {
  #[command(flatten)]
  compile: CompileArgs,
  output: PathBuf,
}

impl VineDocCommand {
  pub fn execute(self) -> Result<()> {
    let mut compiler = self.compile.initialize();
    let result = compiler.compile(());
    if document(&compiler, &self.output).is_err() {
      eprint_diags(&compiler);
      if result.is_ok() {
        eprintln!("could not build docs");
      }
      exit(1);
    }
    Ok(())
  }
}

fn eprint_diags(compiler: &Compiler) {
  _print_diags(stderr(), compiler).unwrap();
}

fn print_diags(compiler: &Compiler) {
  _print_diags(stdout(), compiler).unwrap();
}

fn _print_diags(mut w: impl io::Write + IsTerminal, compiler: &Compiler) -> io::Result<()> {
  let Colors { reset, bold, underline, grey, red, yellow, .. } = colors(&w);

  let errors = compiler.diags.errors.iter().map(|x| (red, "error", x));
  let warnings = compiler.diags.warnings.iter().map(|x| (yellow, "warning", x));

  for (color, severity, diag) in errors.chain(warnings) {
    let Some(span) = diag.span() else { continue };
    writeln!(w, "{color}{severity}{grey}:{reset} {bold}{diag}{reset}")?;
    if let Some(span_str) = compiler.show_span(span) {
      let file = &compiler.loader.files[span.file];
      let start = file.get_pos(span.start);
      let end = file.get_pos(span.end);
      let line_width = (end.line + 1).ilog10() as usize + 1;
      writeln!(w, " {:>line_width$}{grey} @ {span_str}", "")?;
      for line in start.line..=end.line {
        let line_start = file.line_starts[line];
        let line_end = file.line_starts.get(line + 1).copied().unwrap_or(file.src.len());
        let line_str = &file.src[line_start..line_end];
        let line_str =
          line_str.strip_suffix("\r\n").or(line_str.strip_suffix("\n")).unwrap_or(line_str);
        let start = if line == start.line { start.col } else { 0 };
        let end = if line == end.line { end.col } else { line_str.len() };
        writeln!(
          w,
          " {grey}{:>line_width$} |{reset} {}{color}{underline}{}{reset}{}",
          line + 1,
          &line_str[..start],
          &line_str[start..end],
          &line_str[end..],
        )?;
      }
    }
    writeln!(w,)?;
  }

  Ok(())
}

struct Colors {
  reset: &'static str,
  bold: &'static str,
  underline: &'static str,
  grey: &'static str,
  red: &'static str,
  yellow: &'static str,
  green: &'static str,
}

fn colors(t: &impl IsTerminal) -> Colors {
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
