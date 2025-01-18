pub mod analyzer;
pub mod ast;
pub mod chart;
pub mod charter;
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
// pub mod repl;
pub mod finder;
pub mod resolver;
pub mod specializer;
pub mod unifier;
pub mod vir;
pub mod visit;

use std::path::PathBuf;

use analyzer::analyze;
use checker::Checker;
use distiller::Distiller;
use emitter::Emitter;
use ivy::ast::Nets;
use normalizer::normalize;
use resolver::Resolver;
use specializer::specialize;

use crate::{
  chart::Chart,
  charter::Charter,
  core::{Core, CoreArenas},
  loader::Loader,
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

  let chart = &mut Chart::default();

  Charter { core, chart }.chart_root(root);
  Resolver { core, chart }.resolve_all();
  Checker::new(core, chart).check_all();

  core.bail()?;

  let specializations = specialize(chart);
  let mut distiller = Distiller::new(chart);
  let mut emitter = Emitter::new(chart);
  for (value_id, value_def) in &chart.values {
    let path = chart.defs[value_def.def].path;
    if matches_filter(path, &config.items) {
      if let Some(vir) = distiller.distill(value_def) {
        let mut vir = normalize(&vir);
        analyze(&mut vir);
        emitter.emit_vir(path, &vir, &specializations[value_id]);
      } else {
        emitter.emit_ivy(value_def);
      }
    }
  }

  Ok(emitter.nets)
}

fn matches_filter(path: &str, items: &[String]) -> bool {
  if items.is_empty() {
    return true;
  }
  items.iter().any(|x| x == &path[2..])
}
