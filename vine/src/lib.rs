pub mod analyzer;
pub mod ast;
pub mod chart;
pub mod charter;
pub mod checker;
pub mod core;
pub mod diag;
pub mod distiller;
// pub mod emitter;
pub mod fmt;
pub mod lexer;
pub mod loader;
pub mod normalizer;
pub mod parser;
// pub mod repl;
// pub mod specializer;
pub mod resolver;
pub mod vir;
pub mod visit;

mod specializer {
  vine_util::new_idx!(pub RelId);
}

use std::path::PathBuf;

use ivy::ast::Nets;
use resolver::Resolver;

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

  // let mut checker = Checker::new(core, &mut resolver);
  // checker.check_defs();

  // core.bail()?;

  // let specializations = specialize(&mut resolver);
  // let mut distiller = Distiller::new(&resolver);
  // let mut emitter = Emitter::new(&resolver);
  // for (def_id, def) in &resolver.defs {
  //   if matches_filter(&def.canonical, &config.items) {
  //     if let Some(vir) = distiller.distill(def) {
  //       let mut vir = normalize(&vir);
  //       analyze(&mut vir);
  //       emitter.emit_vir(def.canonical.to_string(), &vir,
  // &specializations[def_id]);     } else {
  //       emitter.emit_ivy(def);
  //     }
  //   }
  // }

  // Ok(emitter.nets)

  todo!()
}

// fn matches_filter(path: &Path, items: &[String]) -> bool {
//   if items.is_empty() {
//     return true;
//   }
//   let canonical = &path.to_string()[2..];
//   items.iter().any(|x| x == canonical)
// }
