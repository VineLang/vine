use std::env::args;

use ivy::optimize::Optimizer;
use vine::{compile::Compiler, loader::Loader, resolve::Resolver};
use vine_util::{arena::BytesArena, interner::StringInterner};

fn main() {
  let mut args = args();
  args.next(); // ignore bin path

  let arena = &*Box::leak(Box::new(BytesArena::default()));
  let interner = StringInterner::new(arena);

  let mut loader = Loader::new(&interner);
  loader.load_main_mod(args.next().expect("must supply main mod"));
  for lib in args {
    loader.load_mod(lib);
  }
  let root = loader.finish();

  let mut resolver = Resolver::default();
  resolver.build_graph(root);
  resolver.resolve_terms();

  let mut compiler = Compiler::default();
  for node in &resolver.nodes {
    compiler.compile_node(node)
  }

  let mut nets = compiler.nets;

  Optimizer::default().optimize(&mut nets);

  println!("{nets}");
}
