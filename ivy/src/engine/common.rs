use std::collections::HashSet;

use vine_util::register::Register;

use crate::{
  engine::{Interaction, Rewrite, Rules},
  name::PathId,
};

pub fn graft<'r>(path: PathId, lazy: bool) -> impl Register<Rules<'r>> {
  Rewrite([path], move |engine, _, net, node| {
    if lazy && !engine.follow(node.port(0)).is_principal() {
      return false;
    }
    let graft_name = engine.name(node).children[0];
    let graft_net = engine.get_net(graft_name);
    if !engine.await_net(graft_net, node) {
      return false;
    }
    let a = engine.graft_net(graft_net, net);
    let b = engine.remove_node(node);
    engine.link_all(a, b);
    true
  })
}

pub fn annihilate<'r>(paths: impl IntoIterator<Item = PathId>) -> impl Register<Rules<'r>> {
  Interaction(paths.into_iter().map(|p| (p, p)), |engine, _, _, a, b| {
    if engine.name(a) != engine.name(b) {
      return false;
    }
    if engine.ports(a).len() != engine.ports(b).len() {
      return false;
    }
    let a = engine.remove_node(a);
    let b = engine.remove_node(b);
    engine.link_all(a, b);
    true
  })
}

pub fn erase<'r>(
  erasers: impl IntoIterator<Item = PathId>,
  nodes: impl IntoIterator<Item = PathId> + Clone,
) -> impl Register<Rules<'r>> {
  let pairs = erasers.into_iter().flat_map(move |a| nodes.clone().into_iter().map(move |b| (a, b)));
  Interaction(pairs, |engine, _, net, eraser, node| {
    if engine.ports(eraser).len() != 1 {
      return false;
    }
    let name = engine.name(eraser).clone();
    let [pri] = engine.remove_node_n(eraser);
    let mut ports = engine.remove_node(node);
    engine.remove_wire(pri, ports.next().unwrap());
    for port in ports {
      let [eraser] = engine.insert_node_n(net, name.clone());
      engine.link(port, eraser);
    }
    true
  })
}

pub fn commute<'r>(
  a: impl IntoIterator<Item = PathId>,
  b: impl IntoIterator<Item = PathId> + Clone,
) -> impl Register<Rules<'r>> {
  let pairs = a.into_iter().flat_map(move |a| b.clone().into_iter().map(move |b| (a, b)));
  Interaction(pairs, |engine, _, net, a, b| {
    let a_name = engine.name(a).clone();
    let b_name = engine.name(b).clone();
    let mut a_ports = engine.remove_node(a);
    let mut b_ports = engine.remove_node(b);
    engine.remove_wire(a_ports.next().unwrap(), b_ports.next().unwrap());
    let mut inter = a_ports
      .map(|a| {
        let mut ports = engine.insert_node(net, b_name.clone(), 1 + b_ports.len());
        engine.link(a, ports.next().unwrap());
        ports
      })
      .collect::<Vec<_>>();
    for b in b_ports {
      let mut ports = engine.insert_node(net, a_name.clone(), 1 + inter.len());
      engine.link(b, ports.next().unwrap());
      for (x, y) in ports.zip(inter.iter_mut().map(|x| x.next().unwrap())) {
        engine.link(x, y);
      }
    }
    true
  })
}

pub fn eta_reduce<'r>(
  combinators: impl IntoIterator<Item = PathId>,
  values: impl IntoIterator<Item = PathId>,
  nils: impl IntoIterator<Item = PathId> + Clone,
) -> impl Register<Rules<'r>> {
  let values = HashSet::<_>::from_iter(values.into_iter().chain(nils.clone()));
  let nils = HashSet::<_>::from_iter(nils);
  Rewrite(combinators, move |engine, _, net, node| {
    let aux = engine.ports(node).skip(1);
    if aux.len() == 0 {
      return false;
    }
    let aux_ = aux.clone().map(|p| engine.follow(p));
    if aux_.clone().all(|p| p.is_principal()) {
      let nodes = aux_.map(|p| p.node);
      if !nodes.clone().all(|n| engine.ports(n).len() == 1) {
        return false;
      }
      let mut names = nodes.map(|n| engine.name(n));
      let name = names.next().unwrap();
      if !values.contains(&name.path) || !names.all(|n| n == name) {
        return false;
      }
      let name = name.clone();
      let mut ports = engine.remove_node(node);
      let pri = ports.next().unwrap();
      for port in ports {
        let [port_] = engine.remove_node_n(engine.follow(*port).node);
        engine.remove_wire(port, port_);
      }
      let [pri_] = engine.insert_node_n(net, name);
      engine.link(pri, pri_);
      true
    } else {
      let (a, a_aux) = (node, aux);
      let b = aux_.clone().find(|x| !x.is_principal()).unwrap().node;
      let b_aux = engine.ports(b).skip(1);
      if a_aux.len() != b_aux.len() || engine.name(a) != engine.name(b) {
        return false;
      }
      if !a_aux.zip(b_aux).all(|(p, q)| {
        let p_ = engine.follow(p);
        if p_ == q {
          true
        } else {
          let q_ = engine.follow(q);
          engine.ports(p_.node).len() == 1
            && engine.ports(q_.node).len() == 1
            && engine.name(p_.node) == engine.name(q_.node)
            && nils.contains(&engine.name(p_.node).path)
        }
      }) {
        return false;
      }
      let mut a_ports = engine.remove_node(a);
      let mut b_ports = engine.remove_node(b);
      let a_pri = a_ports.next().unwrap();
      let b_pri = b_ports.next().unwrap();
      engine.link(a_pri, b_pri);
      for (p, q) in a_ports.zip(b_ports) {
        let p_ = engine.follow(*p);
        if p_ == *q {
          engine.remove_wire(p, q);
        } else {
          let q_ = engine.follow(*q);
          let [p_] = engine.remove_node_n(p_.node);
          let [q_] = engine.remove_node_n(q_.node);
          engine.remove_wire(p, p_);
          engine.remove_wire(q, q_);
        }
      }
      true
    }
  })
}

#[cfg(test)]
mod test {
  use super::*;

  use std::fmt;

  use crate::{
    engine::optimize,
    guide,
    name::{FromTable, Table},
    net::TreeNet,
    text::{ast::Nets, parser::Parser},
  };

  #[test]
  fn test_eta() {
    let table = &mut Table::default();
    let before = "
      :yay#0 { ^ = :x(:x :x(:x :x)) }
      :yay#1 { ^ = :x(:n :x(:n :n)) }
      :yay#2 { ^0 = :x(0 1); ^1 = :x(0 1) }
      :yay#3 { ^0 = :x(0 :x); ^1 = :x(0 :x) }
      :yay#4 { ^0 = :x(0 :x(:x :x(:x))); ^1 = :x(0 :x) }

      :nay#0 { ^ = :x(:y :y) }
      :nay#1 { ^ = :x(:n :n#1) }
      :nay#2 { ^0 = :x(0 1); ^1 = :x(1 0) }
      :nay#3 { ^0 = :x(0 1); ^1 = :x#1(0 1) }
      :nay#4 { ^0 = :x(0 :n); ^1 = :x(0 :n) }
      :nay#5 { ^0 = :x(0 :x); ^1 = :x(0 :x#1) }
      :nay#6 { ^0 = :x(0 :y); ^1 = :x(0 :y) }
    ";
    let after = "
      :yay#0 { ^ = :x }
      :yay#1 { ^ = :n }
      :yay#2 { ^0 = 0; ^1 = 0 }
      :yay#3 { ^0 = 0; ^1 = 0 }
      :yay#4 { ^0 = 0; ^1 = 0 }

      :nay#0 { ^ = :x(:y :y) }
      :nay#1 { ^ = :x(:n :n#1) }
      :nay#2 { ^0 = :x(0 1); ^1 = :x(1 0) }
      :nay#3 { ^0 = :x(0 1); ^1 = :x#1(0 1) }
      :nay#4 { ^0 = :x(0 :n); ^1 = :x(0 :n) }
      :nay#5 { ^0 = :x(0 :x); ^1 = :x(0 :x#1) }
      :nay#6 { ^0 = :x(0 :y); ^1 = :x(0 :y) }
    ";
    guide!(pub Guide {
      x: PathId = ":x",
      y: PathId = ":y",
      n: PathId = ":n",
    });
    let guide = Guide::build(table);
    let rules = Rules::new(eta_reduce([guide.x, guide.y], [guide.n], [guide.x]));
    let mut nets = Parser::parse(table, before).unwrap().to_flat_nets().unwrap();
    optimize(table, &rules, &mut nets, &mut None, |_| true);
    let mut nets = TreeNet::from_flat_nets(&nets);
    nets.values_mut().for_each(|n| n.resolve_links());
    let actual = Nets::from_tree_nets(&nets).print(table);
    let expected = Parser::parse(table, after).unwrap().print(table);
    assert_eq!(Raw(actual), Raw(expected));
  }
  #[derive(PartialEq, Eq)]
  struct Raw<T>(T);
  impl<T: fmt::Display> fmt::Debug for Raw<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      self.0.fmt(f)
    }
  }
}
