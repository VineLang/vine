use std::{env::args, fs};

use vine::{compile::Compiler, parser::VineParser, resolve::Resolver};
use vine_util::{arena::BytesArena, interner::StringInterner};

fn main() {
  let path = args().nth(1).expect("must supply path");

  let arena = &*Box::leak(Box::new(BytesArena::default()));
  let interner = StringInterner::new(arena);

  let src = fs::read_to_string(path).unwrap();
  let root = VineParser::parse(&interner, &src).unwrap();

  let mut resolver = Resolver::default();
  resolver.build_graph(root);
  resolver.resolve_terms();

  let mut compiler = Compiler::default();
  for node in &resolver.nodes {
    if let Some(value) = &node.value {
      compiler.compile_global(&node.canonical, value);
    }
  }

  let nets = compiler.nets;
  println!("{nets}");
}
