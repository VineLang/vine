use std::{collections::HashMap, mem::replace};

use ivy::{
  engine::{
    Engine, Interaction, NetId, NodeId, Port, Rewrite, Rules,
    common::{annihilate, commute, erase, eta_reduce, graft},
    optimize,
  },
  guide,
  name::{FromTable, NameId, PathId, Table},
  net::FlatNet,
  translate::{
    Translate, Translator,
    common::{chain_binary, elide_unary},
  },
};
use vine_util::{
  exact_size,
  nat::Nat,
  register::{Register, Registry},
};

use crate::compiler::Guide;

pub fn optimize_vi(
  table: &mut Table,
  nets: &mut HashMap<NameId, FlatNet>,
  limit: &mut Option<usize>,
  filter: impl Fn(NameId) -> bool,
) {
  let vi = &Guide::build(table);
  let opt = &OptGuide::build(table);
  Translator::new(into_opt(vi, opt, table)).translate_all(table, nets);
  let rules = Rules::new(optimizations(vi, opt, table));
  optimize(table, &rules, nets, limit, filter);
  Translator::new(from_opt(vi, opt, table)).translate_all(table, nets);
}

fn into_opt<'r>(
  vi: &'r Guide,
  opt: &'r OptGuide,
  table: &mut Table,
) -> impl Register<Translator<'r>> {
  let strip = fancy_ext(opt)
    .chain(bare_ext(opt).map(|(path, _)| path))
    .map(|path| (table.add_name(path), path))
    .collect::<HashMap<_, _>>();
  (Translate([vi.ext], move |mut node, _, net| {
    if let Some(&path) = strip.get(&node.name.children[0]) {
      node.name = path.into();
    }
    net.push(node);
  }),)
}

fn from_opt<'r>(
  vi: &'r Guide,
  opt: &'r OptGuide,
  table: &mut Table,
) -> impl Register<Translator<'r>> {
  let restore = bare_ext(opt)
    .chain([(opt.n32_add, 2), (opt.n32_mul, 2)])
    .map(|(path, arity)| (path, (table.add_name(path), arity)))
    .collect::<HashMap<_, _>>();
  (
    insert_constant(vi, opt.n32_add, 0),
    chain_binary([opt.n32_add, opt.n32_mul]),
    elide_unary([opt.n32_add, opt.n32_mul]),
    Translate(
      bare_ext(opt).map(|(path, _)| path).chain([opt.n32_add, opt.n32_mul]),
      move |mut node, _, net| {
        let (name_id, arity) = restore[&node.name.path];
        node.name = vi.ext.with_children([name_id]).with_payload(arity as u32);
        net.push(node);
      },
    ),
  )
}

fn insert_constant<'r>(
  vi: &'r Guide,
  path: PathId,
  identity: u32,
) -> impl Register<Translator<'r>> {
  Translate([path], move |mut node, _, net| {
    let payload = replace(&mut node.name.payload, Nat::ZERO);
    if payload.as_u32().unwrap() != identity {
      node.aux.insert(node.aux.len() - 1, net.make(vi.n32.with_payload(payload), []));
    }
    net.push(node);
  })
}

guide!(pub OptGuide {
  n32_add: PathId = "vi:n32:add",
  n32_div: PathId = "vi:n32:div",
  n32_mul: PathId = "vi:n32:mul",
  n32_shr: PathId = "vi:n32:shr",
  n32_shl: PathId = "vi:n32:shl",
});

fn fancy_ext(opt: &OptGuide) -> impl Iterator<Item = PathId> {
  [opt.n32_add, opt.n32_mul].into_iter()
}

fn bare_ext(opt: &OptGuide) -> impl Iterator<Item = (PathId, usize)> {
  [(opt.n32_div, 2), (opt.n32_shr, 2), (opt.n32_shl, 2)].into_iter()
}

fn optimizations<'r>(
  vi: &'r Guide,
  opt: &'r OptGuide,
  table: &mut Table,
) -> impl use<'r> + Register<Rules<'r>> {
  (
    interactions(vi, opt, table),
    Rewrite([opt.n32_div], move |engine, _, net, node| {
      let rhs = engine.follow(node.port(1));
      if !rhs.is_principal() {
        return false;
      }
      let rhs_name = engine.name(rhs.node);
      if rhs_name.path != vi.n32 {
        return false;
      }
      let n = rhs_name.payload.as_u32().unwrap();
      if !n.is_power_of_two() {
        return false;
      }
      let [rhs_] = engine.remove_node_n(rhs.node);
      let [lhs, rhs, out] = engine.remove_node_n(node);
      engine.remove_wire(rhs, rhs_);
      let [lhs_, rhs_, out_] = engine.insert_node_n(net, opt.n32_shr.into());
      let [rhs] = engine.insert_node_n(net, vi.n32.with_payload(n.trailing_zeros()));
      engine.link_all([lhs, rhs, out], [lhs_, rhs_, out_]);
      true
    }),
    regroup(vi, opt.n32_add, |a, b| a.wrapping_add(b)),
    regroup(vi, opt.n32_mul, |a, b| a.wrapping_mul(b)),
    Rewrite([vi.bool_if], move |engine, _, net, node| {
      let name = engine.name(node);
      let cond = engine.follow(node.port(0));
      let cond_name = engine.name(cond.node);
      if cond_name.path != vi.ext {
        return false;
      }
      if cond_name.children[0] != vi.bool_not {
        return false;
      }
      let mut name = name.clone();
      name.children.swap(0, 1);
      let [bool, cond] = engine.remove_node_n(cond.node);
      let [cond_, ctx] = engine.remove_node_n(node);
      engine.remove_wire(cond, cond_);
      let [bool_, ctx_] = engine.insert_node_n(net, name);
      engine.link(bool, bool_);
      engine.link(ctx, ctx_);
      true
    }),
  )
}

fn regroup<'r, F: 'r + Fn(u32, u32) -> u32>(
  vi: &'r Guide,
  path: PathId,
  op: F,
) -> impl use<'r, F> + Register<Rules<'r>> {
  Rewrite([path], move |engine, _, net, node| {
    let ports = engine.ports(node);
    let arity = ports.len() - 1;
    if !engine.ports(node).take(arity).any(|arg_| {
      let arg = engine.follow(arg_);
      if !arg.is_principal() {
        return false;
      }
      let name = engine.name(arg.node);
      name.path == path || name.path == vi.n32
    }) {
      return false;
    }
    let mut value = engine.name(node).payload.as_u32().unwrap();
    let mut args = Vec::new();
    let mut ports = engine.remove_node(node);
    for arg_ in (&mut ports).take(arity) {
      let arg = engine.follow(*arg_);
      let name = engine.name(arg.node);
      if name.path == path || name.path == vi.n32 {
        value = op(value, name.payload.as_u32().unwrap());
        let mut ports = engine.remove_node(arg.node);
        let arg = ports.next_back().unwrap();
        engine.remove_wire(arg, arg_);
        args.extend(ports);
      } else {
        args.push(arg_);
      }
    }
    let args_ = engine.insert_node(
      net,
      if args.is_empty() { vi.n32 } else { path }.with_payload(value),
      args.len() + 1,
    );
    args.push(ports.next().unwrap());
    engine.link_all(args, args_);
    true
  })
}

fn interactions<'r>(
  vi: &'r Guide,
  opt: &'r OptGuide,
  table: &mut Table,
) -> impl use<'r> + Register<Rules<'r>> {
  let vi_data = [vi.bool, vi.n32, vi.f32];
  let vi_x = [vi.tuple, vi.fn_, vi.dbg, vi.ref_, vi.interface];
  (
    graft(vi.graft, false),
    annihilate([vi.dup].into_iter().chain(vi_x)),
    commute(
      [vi.dup],
      [vi.eraser, vi.tuple, vi.enum_variant, vi.enum_match].into_iter().chain(vi_data),
    ),
    erase(
      [vi.eraser].into_iter().chain(vi_x),
      [vi.eraser, vi.enum_variant, vi.enum_match].into_iter().chain(vi_data).chain(vi_x),
    ),
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
      let [ctx_, content_] = engine.insert_node_n(net, vi.graft.with_children([graft_name]));
      engine.link(content, content_);
      engine.link(ctx, ctx_);
      true
    }),
    arithmetic(vi, opt, table),
  )
}

pub fn arithmetic<'r>(
  vi: &'r Guide,
  opt: &'r OptGuide,
  table: &mut Table,
) -> impl use<'r> + Register<Rules<'r>> {
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

  type Fns = HashMap<NameId, Box<dyn Fn(usize, &mut Engine, &Guide, NetId, NodeId) -> bool>>;

  struct Arithmetic;
  impl Registry for Arithmetic {
    type Mut<'r>
      = (&'r mut Fns, &'r mut Table)
    where
      Self: 'r;
    fn fork<'a>((registry, table): &'a mut Self::Mut<'_>) -> Self::Mut<'a> {
      (registry, table)
    }
  }

  impl<const N: usize, const M: usize, I: Inputs<N>, O: Outputs<M>> Define<I, O, [[(); N]; M]>
    for Arithmetic
  {
    fn define(
      (fns, table): Self::Mut<'_>,
      name: &'static str,
      f: impl 'static + Send + Sync + Fn(I) -> O,
    ) {
      let _name = name;
      let name = table.add_path_name(name);
      fns.insert(
        name,
        Box::new(move |arity, engine, vi, net, node| {
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
  Register::<Arithmetic>::register(arithmetic(()), (&mut fns, table));
  Arithmetic::define((&mut fns, table), "vi:bool:and", |(a, b): (bool, bool)| a & b);
  Arithmetic::define((&mut fns, table), "vi:bool:or", |(a, b): (bool, bool)| a | b);
  Arithmetic::define((&mut fns, table), "vi:bool:xor", |(a, b): (bool, bool)| a ^ b);
  Arithmetic::define((&mut fns, table), "vi:bool:eq", |(a, b): (bool, bool)| a == b);
  Arithmetic::define((&mut fns, table), "vi:bool:le", |(a, b): (bool, bool)| a <= b);
  Arithmetic::define((&mut fns, table), "vi:bool:lt", |(a, b): (bool, bool)| !a & b);
  Arithmetic::define((&mut fns, table), "vi:bool:not", |a: bool| !a);
  Arithmetic::define((&mut fns, table), "vi:bool:to_n32", |a: bool| a as u32);

  let bare_ext_lookup = bare_ext(opt)
    .map(|(path, arity)| (path, (table.add_name(path), arity)))
    .collect::<HashMap<_, _>>();

  Rewrite(bare_ext(opt).map(|(path, _)| path).chain([vi.ext]), move |engine, _, net, node| {
    let name = engine.name(node);
    let (name, arity) = if name.path == vi.ext {
      (name.children[0], name.payload.as_u32().unwrap() as usize)
    } else {
      bare_ext_lookup[&name.path]
    };
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
    let Some(f) = fns.get(&name) else { return false };
    f(arity, engine, vi, net, node)
  })
}
