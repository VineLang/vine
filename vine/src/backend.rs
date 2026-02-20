use std::{
  collections::{HashMap, hash_map::Entry},
  slice,
};

use ivm::system::{
  Guide as IvmGuide, construct_f64s, optimizations as ivm_optimizations, replace_ext_fns,
  variadic_ext_fn,
};
use ivy::{
  engine::{
    Engine, Interaction, NetId, NodeId, Port, Rewrite, Rules,
    common::{annihilate, commute, erase, eta_reduce, graft},
    optimize,
  },
  name::{FromTable, NameId, PathId, Table},
  net::FlatNet,
  prune::prune,
  translate::{
    Translate, Translator,
    common::{chain_binary, elide_unary, map_name, replace_name, replace_nilary, replace_path},
  },
};
use vine_util::{exact_size, nat::Nat, register::Register};

use crate::compiler::Guide;

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
  let occurrences = &get_occurrences(config, nets, vi);
  let rules = optimizations(vi, table);
  apply_optimizations(table, config, nets, vi, occurrences, limit, rules);
  match config.target {
    Target::None => {}
    Target::Ivm => {
      let ivm = &IvmGuide::build(table);
      Translator::new(vi_to_ivm(vi, ivm)).translate_all(table, nets);
      apply_optimizations(table, config, nets, vi, occurrences, limit, ivm_optimizations(ivm));
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

fn apply_optimizations<'r>(
  table: &mut Table,
  config: &BackendConfig,
  nets: &mut HashMap<NameId, FlatNet>,
  vi: &Guide,
  occurrences: &HashMap<NameId, Occurrence>,
  limit: &mut Option<usize>,
  rules: impl Register<Rules<'r>>,
) {
  if !config.optimize {
    return;
  }

  let rules = Rules::new(rules);
  optimize(table, &rules, nets, limit, |name| {
    config.optimize_all || occurrences.get(&name) == Some(&Occurrence::Multiple)
  });

  apply_prune(table, config, nets, vi);
}

fn entrypoints<'a>(config: &'a BackendConfig, vi: &'a Guide) -> impl Iterator<Item = NameId> {
  config.entrypoints.as_deref().unwrap_or(slice::from_ref(&vi.main)).iter().copied()
}

#[derive(PartialEq, Eq)]
enum Occurrence {
  Single,
  Multiple,
}

fn get_occurrences(
  config: &BackendConfig,
  nets: &HashMap<NameId, FlatNet>,
  vi: &Guide,
) -> HashMap<NameId, Occurrence> {
  if !config.optimize || config.optimize_all {
    return HashMap::new();
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
  for name in entrypoints(config, vi) {
    visit(nets, name, &mut occurrences);
  }

  occurrences
}

pub fn optimizations<'r>(vi: &'r Guide, table: &mut Table) -> impl use<'r> + Register<Rules<'r>> {
  let vi_data = [vi.bool, vi.n32, vi.f32];
  let vi_x = [vi.tuple, vi.fn_, vi.dbg, vi.ref_, vi.interface];
  (
    graft(vi.graft, false),
    graft(vi.graft_lazy, true),
    annihilate([vi.dup].into_iter().chain(vi_x)),
    commute([vi.dup], [vi.eraser, vi.tuple].into_iter().chain(vi_data)),
    erase([vi.eraser].into_iter().chain(vi_x), [vi.eraser].into_iter().chain(vi_data).chain(vi_x)),
    eta_reduce(vi_x, [vi.eraser], vi_x),
    Interaction([(vi.bool, vi.bool_if)], move |engine, _, net, bool, cond| {
      let value = engine.name(bool).payload.as_u32().unwrap() != 0;
      let graft_name = engine.name(cond).children[!value as usize];
      let [pri] = engine.remove_node_n(bool);
      let [pri_, ctx] = engine.remove_node_n(cond);
      engine.remove_wire(pri, pri_);
      let [graft] = engine.insert_node_n(net, vi.graft.with_children([graft_name]));
      engine.link(graft, ctx);
      true
    }),
    Interaction([(vi.enum_variant, vi.enum_match)], move |engine, _, net, variant, match_| {
      assert_eq!(engine.name(variant).children[0], engine.name(match_).children[0]);
      let tag = engine.name(variant).payload.as_u32().unwrap();
      let graft_name = engine.name(match_).children[tag as usize + 1];
      let [pri, content] = engine.remove_node_n(variant);
      let [pri_, ctx] = engine.remove_node_n(match_);
      engine.remove_wire(pri, pri_);
      let [interface, content_, ctx_] = engine.insert_node_n(net, vi.interface.into());
      let [graft] = engine.insert_node_n(net, vi.graft.with_children([graft_name]));
      engine.link(content, content_);
      engine.link(ctx, ctx_);
      engine.link(graft, interface);
      true
    }),
    arithmetic(vi, table),
  )
}

pub fn vi_to_ivm<'r>(vi: &'r Guide, ivm: &'r IvmGuide) -> impl Register<Translator<'r>> {
  (
    replace_name([vi.ref_, vi.fn_, vi.tuple, vi.dbg, vi.interface], ivm.x),
    replace_name([vi.dup], ivm.y),
    chain_binary([ivm.x, ivm.y]),
    elide_unary([ivm.x, ivm.y]),
    replace_nilary([ivm.x, ivm.y, vi.eraser], ivm.eraser),
    replace_path([vi.graft, vi.graft_lazy], ivm.graft),
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
      let ctx = net.make(ivm.x, [content, ctx]);
      net.add(ivm.branch.with_children(branches), tag, [ctx]);
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

pub fn arithmetic<'r>(vi: &'r Guide, table: &mut Table) -> impl use<'r> + Register<Rules<'r>> {
  use vine_util::arithmetic::{Define, arithmetic};

  trait Value: Sized {
    fn path(vi: &Guide) -> PathId;
    fn from_payload(payload: &Nat) -> Option<Self>;
    fn to_payload(self) -> Nat;

    fn read(vi: &Guide, engine: &Engine, node: NodeId) -> Option<Self> {
      let name = engine.name(node);
      (name.path == Self::path(vi)).then(|| Self::from_payload(&name.payload)).flatten()
    }

    fn write(self, vi: &Guide, engine: &mut Engine, net: NetId, port: Port) {
      let [port_] = engine.insert_node_n(net, Self::path(vi).with_payload(self.to_payload()));
      engine.link(port, port_);
    }
  }

  impl Value for u32 {
    fn path(vi: &Guide) -> PathId {
      vi.n32
    }

    fn from_payload(payload: &Nat) -> Option<Self> {
      payload.as_u32()
    }

    fn to_payload(self) -> Nat {
      self.into()
    }
  }

  impl Value for f32 {
    fn path(vi: &Guide) -> PathId {
      vi.f32
    }

    fn from_payload(payload: &Nat) -> Option<Self> {
      Some(f32::from_bits(payload.as_u32()?))
    }

    fn to_payload(self) -> Nat {
      self.to_bits().into()
    }
  }

  impl Value for f64 {
    fn path(vi: &Guide) -> PathId {
      vi.f64
    }

    fn from_payload(payload: &Nat) -> Option<Self> {
      Some(f64::from_bits(payload.as_u64()?))
    }

    fn to_payload(self) -> Nat {
      self.to_bits().into()
    }
  }

  impl Value for bool {
    fn path(vi: &Guide) -> PathId {
      vi.bool
    }

    fn from_payload(payload: &Nat) -> Option<Self> {
      Some(!payload.is_zero())
    }

    fn to_payload(self) -> Nat {
      self.into()
    }
  }

  trait Inputs<const N: usize>: Sized {
    fn read_all(vi: &Guide, engine: &Engine, nodes: [NodeId; N]) -> Option<Self>;
  }

  trait Outputs<const N: usize>: Sized {
    fn ok(&self) -> bool {
      true
    }
    fn write_all(self, vi: &Guide, engine: &mut Engine, net: NetId, ports: [Port; N]);
  }

  impl<T: Value> Inputs<1> for T {
    fn read_all(vi: &Guide, engine: &Engine, [node]: [NodeId; 1]) -> Option<Self> {
      T::read(vi, engine, node)
    }
  }

  impl<A: Value, B: Value> Inputs<2> for (A, B) {
    fn read_all(vi: &Guide, engine: &Engine, [a, b]: [NodeId; 2]) -> Option<Self> {
      Some((A::read(vi, engine, a)?, B::read(vi, engine, b)?))
    }
  }

  impl Outputs<0> for () {
    fn write_all(self, _: &Guide, _: &mut Engine, _: NetId, []: [Port; 0]) {}
  }

  impl<T: Value> Outputs<1> for T {
    fn write_all(self, vi: &Guide, engine: &mut Engine, net: NetId, [port]: [Port; 1]) {
      self.write(vi, engine, net, port);
    }
  }

  impl<A: Value, B: Value> Outputs<2> for (A, B) {
    fn write_all(self, vi: &Guide, engine: &mut Engine, net: NetId, [a, b]: [Port; 2]) {
      self.0.write(vi, engine, net, a);
      self.1.write(vi, engine, net, b);
    }
  }

  impl<const N: usize, T: Outputs<N>> Outputs<N> for Result<T, ()> {
    fn ok(&self) -> bool {
      self.as_ref().is_ok_and(|o| o.ok())
    }
    fn write_all(self, vi: &Guide, engine: &mut Engine, net: NetId, ports: [Port; N]) {
      self.unwrap().write_all(vi, engine, net, ports);
    }
  }

  struct Registry<'a> {
    table: &'a mut Table,
    fns: &'a mut HashMap<NameId, Box<dyn Fn(&mut Engine, &Guide, NetId, NodeId) -> bool>>,
  }

  impl<const N: usize, const M: usize, I: Inputs<N>, O: Outputs<M>> Define<I, O, [[(); N]; M]>
    for Registry<'_>
  {
    fn define(&mut self, name: &'static str, f: impl 'static + Send + Sync + Fn(I) -> O) {
      let name = self.table.add_path_name(name);
      self.fns.insert(
        name,
        Box::new(move |engine, vi, net, node| {
          let name = engine.name(node);
          let arity = name.payload.as_u32().unwrap() as usize;
          let inputs = engine.ports(node).take(arity).map(|p| engine.follow(p).node);
          let Some(inputs) = I::read_all(vi, engine, exact_size(inputs)) else { return false };
          let outputs = f(inputs);
          if !outputs.ok() {
            return false;
          }
          let mut ports = engine.remove_node(node);
          for input_ in (&mut ports).take(arity) {
            let input = engine.follow(*input_);
            let [input] = engine.remove_node_n(input.node);
            engine.remove_wire(input, input_);
          }
          outputs.write_all(vi, engine, net, exact_size(ports));
          true
        }),
      );
    }
  }

  let mut fns = HashMap::new();
  let mut registry = Registry { table, fns: &mut fns };
  arithmetic(()).register(&mut registry);
  registry.define("vi:bool:and", |(a, b): (bool, bool)| a & b);
  registry.define("vi:bool:or", |(a, b): (bool, bool)| a | b);
  registry.define("vi:bool:xor", |(a, b): (bool, bool)| a ^ b);
  registry.define("vi:bool:eq", |(a, b): (bool, bool)| a == b);
  registry.define("vi:bool:le", |(a, b): (bool, bool)| a <= b);
  registry.define("vi:bool:lt", |(a, b): (bool, bool)| !a & b);
  registry.define("vi:bool:not", |a: bool| !a);
  registry.define("vi:bool:to_n32", |a: bool| a as u32);

  Rewrite([vi.ext], move |engine, _, net, node| {
    let name = engine.name(node);
    let arity = name.payload.as_u32().unwrap() as usize;
    let inputs = engine.ports(node).take(arity).map(|p| engine.follow(p));
    if !inputs.clone().all(|i| i.is_principal()) {
      return false;
    }
    if inputs.clone().any(|i| engine.name(i.node).path == vi.eraser) {
      let ports = engine.remove_node(node);
      for port in ports {
        let [eraser] = engine.insert_node_n(net, vi.eraser.into());
        engine.link(port, eraser);
      }
      return true;
    }
    let Some(f) = fns.get(&name.children[0]) else { return false };
    f(engine, vi, net, node)
  })
}
