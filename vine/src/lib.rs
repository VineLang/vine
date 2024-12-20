pub mod ast;
pub mod checker;
pub mod core;
pub mod diag;
pub mod distiller;
pub mod emitter;
pub mod fmt;
pub mod lexer;
pub mod loader;
pub mod parser;
pub mod repl;
pub mod resolver;
pub mod vir;
pub mod visit;

use core::{Core, CoreArenas};
use std::path::PathBuf;

use checker::Checker;
use ivy::ast::Nets;

use crate::{emitter::emit, loader::Loader, resolver::Resolver};

pub struct Config {
  pub main: Option<PathBuf>,
  pub libs: Vec<PathBuf>,
  pub items: Vec<String>,
}

pub fn compile(config: Config) -> Result<Nets, String> {
  let arenas = CoreArenas::default();
  let core = &Core::new(&arenas);

  let mut loader = Loader::new(core);
  if let Some(main) = config.main {
    loader.load_main_mod(main);
  }
  for lib in config.libs {
    loader.load_mod(lib);
  }

  core.bail()?;

  let root = loader.finish();

  let mut resolver = Resolver::new(core);
  resolver.build_graph(root);
  resolver.resolve_imports();
  resolver.resolve_defs();

  let mut checker = Checker::new(core, &mut resolver);
  checker.check_defs();

  core.bail()?;

  Ok(emit(&resolver, &config.items))
}
