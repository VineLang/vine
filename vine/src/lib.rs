pub mod ast;
pub mod compile;
pub mod desugar;
pub mod loader;
pub mod parser;
pub mod resolve;
pub mod visit;

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

pub fn compiler(config: Config) -> Nets {
  let arena = &*Box::leak(Box::new(BytesArena::default()));
  let interner = StringInterner::new(arena);

  let mut loader = Loader::new(&interner);
  if let Some(main) = config.main {
    loader.load_main_mod(main);
  }
  for lib in config.libs {
    loader.load_mod(lib);
  }
  let root = loader.finish();

  let mut resolver = Resolver::default();
  resolver.build_graph(root);
  resolver.resolve_terms();

  for node in &mut resolver.nodes {
    Desugar.visit_node(node)
  }

  compile(&resolver.nodes, &config.items)
}
