pub mod analyzer;
pub mod ast;
pub mod checker;
pub mod core;
pub mod diag;
pub mod distiller;
pub mod emitter;
pub mod fmt;
pub mod lexer;
pub mod loader;
pub mod normalizer;
pub mod parser;
pub mod repl;
pub mod resolver;
pub mod vir;
pub mod visit;

use core::{Core, CoreArenas};
use std::path::PathBuf;

use ivy::ast::Nets;

use crate::{
  analyzer::analyze, ast::Path, checker::Checker, distiller::Distiller, emitter::Emitter,
  loader::Loader, normalizer::normalize, resolver::Resolver,
};

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

  let mut distiller = Distiller::new(&resolver);
  let mut emitter = Emitter::new(&resolver);
  for (_, def) in &resolver.defs {
    if matches_filter(&def.canonical, &config.items) {
      if let Some(vir) = distiller.distill(def) {
        let mut vir = normalize(&vir);
        analyze(&mut vir);
        // dbg!(&vir);
        emitter.emit_vir(def.canonical.to_string(), &vir);
      } else {
        emitter.emit_ivy(def);
      }
    }
  }

  Ok(emitter.nets)
}

fn matches_filter(path: &Path, items: &[String]) -> bool {
  if items.is_empty() {
    return true;
  }
  let canonical = &path.to_string()[2..];
  items.iter().any(|x| x == canonical)
}
