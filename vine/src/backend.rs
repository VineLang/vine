use std::{
  collections::{HashMap, hash_map::Entry},
  slice,
};

use ivm::system::{
  Guide as IvmGuide, construct_f64s, optimizations as ivm_optimizations, replace_ext_fns,
  variadic_ext_fn,
};
use ivy::{
  engine::{Rules, optimize},
  name::{FromTable, NameId, Table},
  net::FlatNet,
  prune::prune,
  translate::{
    Translate, TranslateFree, Translator,
    common::{chain_binary, elide_unary, map_name, replace_name, replace_nilary, replace_path},
  },
};
use vine_util::register::Register;

use crate::{compiler::Guide, optimizer::optimize_vi};

pub enum Target {
  None,
  Ivm,
}

pub struct BackendConfig {
  pub target: Target,
  pub optimize: bool,
  pub optimize_all: bool,
  pub optimize_limit: Option<usize>,
  pub prune: bool,
  pub entrypoints: Option<Vec<NameId>>,
}

pub fn backend(table: &mut Table, config: &BackendConfig, nets: &mut HashMap<NameId, FlatNet>) {
  let vi = &Guide::build(table);
  let limit = &mut { config.optimize_limit };
  apply_prune(table, config, nets, vi);
  let filter = optimization_filter(config, nets, vi);
  optimize_vi(table, nets, limit, &filter);
  match config.target {
    Target::None => {}
    Target::Ivm => {
      let ivm = &IvmGuide::build(table);
      Translator::new(vi_to_ivm(vi, ivm)).translate_all(table, nets);
      if config.optimize {
        optimize(table, &Rules::new(ivm_optimizations(ivm)), nets, limit, &filter);
      }
    }
  }
  if cfg!(debug_assertions) {
    FlatNet::assert_valid(nets, table);
  }
}

fn apply_prune(
  table: &Table,
  config: &BackendConfig,
  nets: &mut HashMap<NameId, FlatNet>,
  vi: &Guide,
) {
  if config.prune {
    prune(table, nets, entrypoints(config, vi));
  }
}

fn entrypoints<'a>(config: &'a BackendConfig, vi: &'a Guide) -> impl Iterator<Item = NameId> {
  config.entrypoints.as_deref().unwrap_or(slice::from_ref(&vi.main)).iter().copied()
}

fn optimization_filter<'c>(
  config: &'c BackendConfig,
  nets: &HashMap<NameId, FlatNet>,
  vi: &Guide,
) -> impl use<'c> + Fn(NameId) -> bool {
  #[derive(PartialEq, Eq)]
  enum Occurrence {
    Single,
    Multiple,
  }

  fn visit(
    nets: &HashMap<NameId, FlatNet>,
    name: NameId,
    occurrences: &mut HashMap<NameId, Occurrence>,
  ) {
    match occurrences.entry(name) {
      Entry::Occupied(mut e) => match e.get() {
        Occurrence::Multiple => return,
        Occurrence::Single => *e.get_mut() = Occurrence::Multiple,
      },
      Entry::Vacant(e) => {
        e.insert(Occurrence::Single);
      }
    }
    if let Some(net) = nets.get(&name) {
      for node in &net.nodes {
        for &child in &node.name.children {
          if nets.contains_key(&child) {
            visit(nets, child, occurrences);
          }
        }
      }
    }
  }

  let mut occurrences = HashMap::new();
  if config.optimize && !config.optimize_all {
    for name in entrypoints(config, vi) {
      visit(nets, name, &mut occurrences);
    }
  }

  move |name| config.optimize_all || occurrences.get(&name) == Some(&Occurrence::Multiple)
}

pub fn vi_to_ivm<'r>(vi: &'r Guide, ivm: &'r IvmGuide) -> impl Register<Translator<'r>> {
  (
    replace_name([vi.ref_, vi.fn_, vi.tuple, vi.dbg, vi.interface], ivm.x),
    replace_name([vi.dup], ivm.y),
    chain_binary([ivm.x, ivm.y]),
    elide_unary([ivm.x, ivm.y]),
    replace_nilary([ivm.x, ivm.y, vi.eraser], ivm.eraser),
    replace_nilary([vi.error], ivm.error),
    replace_path([vi.graft], ivm.graft),
    replace_path([vi.n32, vi.bool], ivm.n32),
    replace_path([vi.f32], ivm.f32),
    map_name([vi.bool_if], move |_, name| {
      ivm.branch.with_children([name.children[1], name.children[0]])
    }),
    Translate([vi.enum_variant], move |node, _, net| {
      let tag = net.make(ivm.n32.with_payload(node.name.payload), []);
      net.add(ivm.x, node.pri, [tag, node.aux[0]]);
    }),
    Translate([vi.enum_match], move |mut node, _, net| {
      let enum_ = node.pri;
      let [ctx] = *node.aux else { unreachable!() };
      node.name.children.remove(0); // remove first child, which specifies the enum
      let branches = node.name.children;
      let [tag, content] = net.wires();
      net.add(ivm.x, enum_, [tag, content]);
      let ctx = net.make(ivm.x, [ctx, content]);
      net.add(ivm.branch.with_children(branches), tag, [ctx]);
    }),
    TranslateFree(move |free, _, net| {
      if free.len() == 2 {
        let free = net.make(ivm.x, free);
        net.free = vec![free];
      } else {
        net.free = free;
      }
    }),
    Translate([ivm.branch], move |node, _, net| {
      let [ctx] = *node.aux else { unreachable!() };
      let branches = net.make(node.name, []);
      net.add(ivm.ext_merge.with_children([ivm.branch_]), node.pri, [branches, ctx]);
    }),
    Translate([vi.ext], move |mut node, _, net| {
      if node.name.children[0] == vi.bool_not {
        let bool = net.make(ivm.n32.with_payload(1u32), []);
        node.aux.insert(0, bool);
        node.name = vi.ext.with_payload(2u32).with_children([vi.n32_xor]);
        net.push(node);
      } else if node.name.children[0] == vi.bool_to_n32 {
        net.link(node.pri, node.aux[0]);
      } else {
        net.push(node);
      }
    }),
    construct_f64s(ivm, vi.f64, vi.f64_from_bits),
    variadic_ext_fn(ivm, vi.ext),
    replace_ext_fns(
      ivm,
      [
        (vi.bool_and, vi.n32_and),
        (vi.bool_or, vi.n32_or),
        (vi.bool_xor, vi.n32_xor),
        (vi.bool_eq, vi.n32_eq),
        (vi.bool_le, vi.n32_le),
        (vi.bool_lt, vi.n32_lt),
      ],
    ),
  )
}
