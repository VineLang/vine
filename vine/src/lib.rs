pub mod ast;
pub mod compile;
pub mod desugar;
pub mod loader;
pub mod parser;
pub mod repl;
pub mod resolve;
pub mod visit;

pub mod diag;
mod lexer;

use std::path::PathBuf;

use ivy::ast::Nets;
use vine_util::{arena::BytesArena, interner::StringInterner};

use crate::{
  compile::compile, desugar::Desugar, loader::Loader, resolve::Resolver, visit::VisitMut,
};

pub struct Config {
  pub main: Option<PathBuf>,
  pub libs: Vec<PathBuf>,
  pub items: Vec<String>,
}

pub fn build(config: Config) -> Result<Nets, String> {
  let arena = &*Box::leak(Box::new(BytesArena::default()));
  let interner = StringInterner::new(arena);

  let mut loader = Loader::new(&interner);
  if let Some(main) = config.main {
    loader.load_main_mod(main);
  }
  for lib in config.libs {
    loader.load_mod(lib);
  }

  loader.diags.report(&loader.files)?;

  let root = loader.finish();

  let mut resolver = Resolver::default();
  resolver.build_graph(root);
  resolver.resolve_imports();
  resolver.resolve_exprs();

  resolver.diags.report(&loader.files)?;

  for node in &mut resolver.nodes {
    Desugar.visit_node(node)
  }

  Ok(compile(&resolver.nodes, &config.items))
}
