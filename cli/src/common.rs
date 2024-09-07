use ivm::{heap::Heap, stats::Stats, IVM};
use ivy::{ast::Nets, optimize::Optimizer};
use std::path::PathBuf;
use vine::{
  compile::compile, desugar::Desugar, loader::Loader, resolve::Resolver, visit::VisitMut,
};
use vine_util::{arena::BytesArena, interner::StringInterner};

pub fn compile_nets(src: String) -> Nets {
  let arena = &*Box::leak(Box::new(BytesArena::default()));
  let interner = StringInterner::new(arena);

  let mut loader = Loader::new(&interner);
  loader.load_main_mod(src);
  let mut manifest_path = PathBuf::new();
  manifest_path.push(env!("CARGO_MANIFEST_DIR"));
  manifest_path.push("../vine/std/std.vi");
  loader.load_mod(manifest_path);
  let root = loader.finish();

  let mut resolver = Resolver::default();
  resolver.build_graph(root);
  resolver.resolve_terms();

  for node in &mut resolver.nodes {
    Desugar.visit_node(node)
  }

  let mut nets = compile(&resolver.nodes);

  Optimizer::default().optimize(&mut nets);
  nets
}

pub fn run_nets(nets: Nets) {
  let mut globals = Vec::new();
  let globals = nets.serialize(&mut globals);
  let main = &globals[nets.get_index_of("::main").expect("missing main")];
  let heap = Heap::new();
  let mut ivm = IVM::new(&heap);
  ivm.boot(main);
  ivm.normalize();
  eprintln!("{}", ivm.stats);
}