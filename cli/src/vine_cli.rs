use std::{
  collections::HashMap,
  env,
  ffi::OsStr,
  fmt, fs,
  io::{self, IsTerminal, Read, Write, stderr, stdin, stdout},
  path::{Path, PathBuf},
  process::exit,
  sync::mpsc,
  time::{Duration, Instant},
};

use anyhow::Result;
use clap::{
  Args, CommandFactory, Parser,
  builder::{TypedValueParser, ValueParserFactory},
  error::ErrorKind,
};

use ivm::{IVM, ext::Extrinsics, heap::Heap};
use ivy::{ast::Nets, host::Host};
use notify::{RecommendedWatcher, RecursiveMode, Watcher};
use rustyline::DefaultEditor;
use vine::{
  compiler::Compiler,
  components::loader::{FileId, Loader, RealFS},
  structures::{ast::Ident, resolutions::FragmentId},
  tools::{doc::document, fmt::Formatter, repl::Repl},
};
use vine_lsp::lsp;
use vine_util::idx::IdxVec;

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
pub struct LibArgs {
  #[arg(long = "lib")]
  libs: Vec<Source>,
  #[arg(long)]
  no_root: bool,
}

impl LibArgs {
  fn initialize(mut self, compiler: &mut Compiler) -> IdxVec<FileId, PathBuf> {
    if !self.no_root {
      self.libs.push(root_source())
    }

    let mut file_paths = IdxVec::new();

    let mut loader = Loader::new(compiler, RealFS, Some(&mut file_paths));
    for lib in self.libs {
      loader.load_mod(lib.name, lib.path);
    }

    file_paths
  }
}

#[derive(Debug, Args)]
pub struct CheckArgs {
  entrypoints: Vec<String>,
  #[command(flatten)]
  libs: LibArgs,
}

#[derive(Debug, Args)]
pub struct CompileArgs {
  main: Source,
  #[command(flatten)]
  libs: LibArgs,
  #[arg(long)]
  debug: bool,
  #[arg(long, num_args = 2, value_names = ["KEY", "VALUE"])]
  config: Vec<String>,
}

impl CompileArgs {
  fn initialize(self) -> Compiler {
    let mut compiler = Compiler::new(self.debug, build_config(self.config));
    self.libs.initialize(&mut compiler);

    let mut loader = Loader::new(&mut compiler, RealFS, None);
    loader.load_main_mod(self.main.name, self.main.path);

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

fn root_source() -> Source {
  let mut path = env::current_exe().unwrap();
  path.pop();
  let compile_time_root_path = option_env!("VINE_ROOT_PATH");
  let runtime_root_path = env::var("VINE_ROOT_PATH").ok();

  match runtime_root_path.as_deref().or(compile_time_root_path) {
    Some(root_path) => {
      path.push(root_path);
    }
    None => {
      path.push(env!("CARGO_MANIFEST_DIR"));
      path.pop();
      path.push("root");
    }
  }

  Source { name: Ident("root".into()), path }
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
    self.optimizations.apply(&mut nets, &[]);
    self.run_args.check(&nets, !debug);
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
  /// By default, stdout is captured when running test entrypoints. It is only
  /// printed to stderr when an error occurs. Setting `no_capture` always prints
  /// stdout to stderr after the test runs.
  #[arg(short, long)]
  no_capture: bool,
  /// The test patterns to run. Test paths containing any of the provided
  /// strings as substrings are run. If none are provided, all tests are run.
  #[arg(long = "test")]
  tests: Vec<String>,
}

impl VineTestCommand {
  pub fn execute(mut self) -> Result<()> {
    self.compile.debug = true;

    let (mut nets, mut compiler) = self.compile.compile();

    let tests = Self::matching_tests(&self.tests, &compiler);
    eprintln!("running {} test(s)\n", tests.len());

    let entrypoints: Vec<_> = tests.iter().map(|(_, id)| compiler.entrypoint_name(*id)).collect();
    self.optimizations.apply(&mut nets, &entrypoints);

    let Colors { reset, grey, bold, red, green, .. } = colors(&stderr());

    let mut failed = false;
    for (path, test_id) in tests {
      compiler.insert_main_net(&mut nets, test_id);

      eprint!("{grey}test{reset} {bold}{path}{reset} {grey}...{reset} ");
      let (result, output) = self.run_args.output(&nets);
      if result.success() {
        eprintln!("{green}ok{reset}");
      } else {
        failed = true;
        eprintln!("{red}FAILED{reset}");
      }
      if self.no_capture || !result.success() {
        io::stderr().write_all(&output)?;
        eprintln!();
      }
    }

    if failed {
      exit(1);
    }

    Ok(())
  }

  fn matching_tests(tests: &[String], compiler: &Compiler) -> Vec<(String, FragmentId)> {
    compiler
      .chart
      .tests
      .iter()
      .filter_map(|&test_id| {
        let def_id = compiler.chart.concrete_fns[test_id].def;
        let path = &compiler.chart.defs[def_id].path;
        (tests.is_empty() || tests.iter().any(|test| path.contains(test)))
          .then(|| (path.to_owned(), compiler.resolutions.fns[test_id]))
      })
      .collect()
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
    self.optimizations.apply(&mut nets, &[]);
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
  pub fn execute(self) -> Result<()> {
    let mut compiler = Compiler::default();
    self.check.libs.initialize(&mut compiler);

    if compiler.check(()).is_err() {
      eprint_diags(&compiler);
      exit(1);
    }

    let checkpoint = compiler.checkpoint();

    let Colors { reset, red, yellow, green, grey, .. } = colors(&stderr());

    let mut check = move || {
      let start = Instant::now();

      compiler.revert(&checkpoint);

      let mut loader = Loader::new(&mut compiler, RealFS, None);
      for glob in &self.check.entrypoints {
        for entry in glob::glob(glob).unwrap() {
          let path = entry.unwrap();
          if let Some(name) = RealFS::detect_name(&path) {
            loader.load_mod(name, path);
          }
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
  #[command(flatten)]
  libs: LibArgs,
  #[arg(long)]
  echo: bool,
  #[arg(long)]
  no_debug: bool,
  args: Vec<String>,
  #[arg(long, num_args = 2)]
  config: Vec<String>,
}

impl VineReplCommand {
  pub fn execute(self) -> Result<()> {
    let host = &mut Host::default();
    let heap = Heap::new();
    let mut extrinsics = Extrinsics::default();
    host.register_default_extrinsics(&mut extrinsics);
    host.register_runtime_extrinsics(&mut extrinsics, &self.args, io::stdin, io::stdout);

    let mut ivm = IVM::new(&heap, &extrinsics);
    let mut compiler = Compiler::new(!self.no_debug, build_config(self.config));
    self.libs.initialize(&mut compiler);
    let mut repl = match Repl::new(host, &mut ivm, &mut compiler) {
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
pub struct VineFmtCommand {
  files: Option<Vec<PathBuf>>,
}

impl VineFmtCommand {
  pub fn execute(self) -> Result<()> {
    let mut success = true;
    if let Some(files) = self.files {
      for path in files {
        let src = fs::read_to_string(&path)?;
        if let Some(out) = Self::fmt(path.display(), &src) {
          fs::write(&path, out)?;
        } else {
          success = false;
        }
      }
    } else {
      let mut src = String::new();
      stdin().read_to_string(&mut src)?;
      if let Some(out) = Self::fmt("input", &src) {
        print!("{out}");
      } else {
        success = false;
      }
    }
    if !success {
      exit(1);
    }
    Ok(())
  }

  fn fmt(path: impl fmt::Display, src: &str) -> Option<String> {
    match Formatter::fmt(src) {
      Ok(out) => Some(out),
      Err(diag) => {
        eprintln!("error formatting {path}:\n{diag}");
        None
      }
    }
  }
}

#[derive(Debug, Args)]
pub struct VineLspCommand {
  #[command(flatten)]
  check: CheckArgs,
}

impl VineLspCommand {
  pub fn execute(self) -> Result<()> {
    let mut compiler = Compiler::default();
    let file_paths = self.check.libs.initialize(&mut compiler);
    lsp(compiler, file_paths, self.check.entrypoints);
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
      let file = &compiler.files[span.file];
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

#[derive(Clone, Debug)]
struct Source {
  name: Ident,
  path: PathBuf,
}

impl ValueParserFactory for Source {
  type Parser = ParseSource;
  fn value_parser() -> Self::Parser {
    ParseSource
  }
}

#[derive(Clone)]
struct ParseSource;

impl TypedValueParser for ParseSource {
  type Value = Source;

  fn parse_ref(
    &self,
    cmd: &clap::Command,
    _: Option<&clap::Arg>,
    value: &OsStr,
  ) -> Result<Self::Value, clap::Error> {
    let bytes = value.as_encoded_bytes();
    if let Some(index) = bytes.iter().position(|&x| x == b'=') {
      let name = unsafe { OsStr::from_encoded_bytes_unchecked(&bytes[0..index]) };
      let path = unsafe { OsStr::from_encoded_bytes_unchecked(&bytes[index + 1..]) };
      if let Some(name) = name.to_str().and_then(Ident::new) {
        Ok(Source { name, path: path.to_owned().into() })
      } else {
        Err(
          clap::Error::raw(
            ErrorKind::ValueValidation,
            format!("invalid source name `{}`", name.display()),
          )
          .with_cmd(cmd),
        )
      }
    } else {
      let path: &Path = value.as_ref();
      if let Some(name) = RealFS::detect_name(path) {
        Ok(Source { name, path: path.to_owned() })
      } else {
        Err(
          clap::Error::raw(
            ErrorKind::ValueValidation,
            format!("could not detect name for source with path `{}`", path.display()),
          )
          .with_cmd(cmd),
        )
      }
    }
  }
}

fn build_config(args: Vec<String>) -> HashMap<String, String> {
  let mut args = args.into_iter();
  let mut config = HashMap::new();
  while args.len() != 0 {
    config.insert(args.next().unwrap(), args.next().unwrap());
  }
  config
}
