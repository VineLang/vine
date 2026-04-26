use std::{
  collections::{HashMap, hash_map::Entry},
  iter,
  mem::take,
  ops::Deref,
  rc::Rc,
};

use vine_util::{
  exact_size,
  idx::{IdxSlab, IdxVec},
  new_idx,
  register::Register,
};

use crate::{
  name::{Name, NameId, PathId, Table},
  net::{FlatNet, FlatNode, Wire},
};

pub mod common;

pub fn optimize(
  table: &mut Table,
  rules: &Rules,
  nets: &mut HashMap<NameId, FlatNet>,
  limit: &mut Option<usize>,
  filter: impl Fn(NameId) -> bool,
) {
  let mut engine = Engine::default();
  engine.add_flat_nets(table, nets, filter);
  engine.run(table, rules, limit);
  engine.finish(nets);
}

#[derive(Default, Debug)]
pub struct Engine {
  /// All nets under reduction. All nodes are stored together, so each net only
  /// stores the set of nodes it contains.
  nets: IdxVec<NetId, Net>,
  net_lookup: HashMap<NameId, NetId>,

  /// All nodes from all nets under reduction. Each node identifies the `NetId`
  /// it is a part of.
  nodes: IdxSlab<NodeId, Node>,

  /// The number of `Node.ports` entries which are `None`. When this is zero,
  /// the net is fully connected. When this is non-zero, a rewrite is ongoing.
  holes: usize,
  /// Nodes which are scheduled to be removed by `finish_net`, once all of their
  /// ports have been relinked. When this is non-empty, a rewrite is ongoing.
  dead_nodes: Vec<NodeId>,

  /// A list of nodes that may be dirty. Some entries may be invalidated, so
  /// whenever an element is read from here, it must be checked that it still
  /// corresponds to a dirty node.
  maybe_dirty: Vec<NodeId>,
  /// The number of invalidated entries in `maybe_dirty`. If this ever goes
  /// above half the capacity of `maybe_dirty`, `tighten_dirty` will be called
  /// to filter out all invalidated entries. This means that at most half of the
  /// space of `maybe_dirty` is wasted on invalidated entries, whilst making
  /// invalidating a node O(1) amortized.
  not_dirty: usize,
}

#[derive(Debug)]
pub struct Net {
  /// The name of this net.
  name: NameId,

  /// Every net has some set of free ports. If these free ports were
  /// disconnected, it would greatly complicate all of the graph rewriting
  /// logic. Thus, instead, each net has a 'free node', whose ports are
  /// connected to the free ports of the net.
  free: NodeId,

  /// A list of nodes which may be in this net. Some entries may be invalidated,
  /// so whenever an element is read from here, it must be checked that it still
  /// corresponds to a node in this net.
  maybe_nodes: Vec<NodeId>,
  /// The number of invalidated entries in `maybe_nodes`. If this ever goes
  /// above half the capacity of `maybe_nodes`, `tighten_net` will be called to
  /// filter out all invalidated entries. This means that at most half of the
  /// space of `maybe_net` is wasted on invalidated entries, whilst making
  /// invalidating a node O(1) amortized.
  not_nodes: usize,

  /// The number of nodes in this net that are dirty or waiting on another net
  /// to finish reducing. When this is zero, the net has been fully reduced.
  pending: usize,
  /// A list of nodes from other nets that are waiting on this net to finish
  /// reduction. When this net's `pending` becomes zero, all awaiting nodes will
  /// be marked dirty.
  awaiting: Vec<(NodeId, NetId)>,
}

#[derive(Debug)]
pub struct Node {
  net: NetId,
  name: Name,
  dirty: bool,
  ports: IdxVec<PortIndex, Option<Port>>,
}

/// A reference to a port in the net. This does not convey ownership, so it
/// cannot be e.g. linked to other ports, but it can be used to traverse and
/// query the net.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PortRef {
  pub node: NodeId,
  pub index: PortIndex,
}

impl PortRef {
  pub fn is_principal(self) -> bool {
    self.index.is_principal()
  }
}

/// An owned port in the net. This port must get linked to another port;
/// dropping it is a logic error.
#[derive(Debug)]
#[must_use]
pub struct Port(PortRef);

impl Port {
  fn _clone(&self) -> Self {
    Self(self.0)
  }
}

impl Deref for Port {
  type Target = PortRef;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

new_idx!(pub NetId);
new_idx!(pub NodeId);
new_idx!(pub PortIndex);

impl NodeId {
  pub fn port(self, index: impl Into<PortIndex>) -> PortRef {
    PortRef { node: self, index: index.into() }
  }
}

impl PortIndex {
  pub fn is_principal(self) -> bool {
    self == PortIndex(0)
  }
}

impl Engine {
  pub fn follow(&self, p: PortRef) -> PortRef {
    **self.nodes[p.node].ports[p.index].as_ref().unwrap()
  }

  pub fn ports(&self, node: NodeId) -> impl use<> + ExactSizeIterator<Item = PortRef> + Clone {
    self.nodes[node].ports.keys().map(move |i| node.port(i))
  }

  fn _other_port(&mut self, p: PortRef) -> &mut Option<Port> {
    &mut self.nodes[p.node].ports[p.index]
  }

  pub fn link(&mut self, a: Port, b: Port) {
    assert_eq!(self.nodes[a.node].net, self.nodes[b.node].net);

    let mut take = |p: Port| match self._other_port(*p).take() {
      Some(p) => {
        self.holes += 1;
        p
      }
      None => {
        self.holes -= 1;
        p
      }
    };

    let a = take(a);

    if *a == *b {
      _ = self._other_port(*b).take().unwrap();
      self.holes += 1;
      return;
    }

    let b = take(b);

    let ar = *a;
    let br = *b;

    *self._other_port(ar) = Some(b);
    *self._other_port(br) = Some(a);

    self.mark_dirty(ar.node);
    self.mark_dirty(br.node);
  }

  pub fn link_all(
    &mut self,
    a: impl ExactSizeIterator<Item = Port>,
    b: impl ExactSizeIterator<Item = Port>,
  ) {
    assert_eq!(a.len(), b.len());
    for (a, b) in a.zip(b) {
      self.link(a, b);
    }
  }

  pub fn remove_wire(&mut self, a: Port, b: Port) {
    assert_eq!(*self._other_port(*a).take().unwrap(), *b);
    assert_eq!(*self._other_port(*b).take().unwrap(), *a);
    self.holes += 2;
  }

  fn mark_dirty(&mut self, id: NodeId) {
    let node = &mut self.nodes[id];
    if !node.dirty {
      node.dirty = true;
      self.maybe_dirty.push(id);
      self.nets[node.net].pending += 1;
    }
  }

  pub fn name(&self, id: NodeId) -> &Name {
    &self.nodes[id].name
  }

  pub fn insert_node(
    &mut self,
    net: NetId,
    name: Name,
    len: usize,
  ) -> impl use<> + ExactSizeIterator<Item = Port> {
    self._insert_node(net, name, len).1
  }

  pub fn insert_node_n<const N: usize>(&mut self, net: NetId, name: Name) -> [Port; N] {
    exact_size(self._insert_node(net, name, N).1)
  }

  fn _insert_node(
    &mut self,
    net: NetId,
    name: Name,
    len: usize,
  ) -> (NodeId, impl ExactSizeIterator<Item = Port> + use<>) {
    self.holes += len;
    let node = self.nodes.insert(Node { net, name, dirty: true, ports: IdxVec::new() });
    self.maybe_dirty.push(node);
    self.nodes[node].ports = (0..len).map(|_| None).collect();
    let net = &mut self.nets[net];
    net.maybe_nodes.push(node);
    net.pending += 1;
    (node, (0..len).map(move |i| Port(PortRef { node, index: PortIndex(i) })))
  }

  pub fn remove_node(&mut self, node_id: NodeId) -> impl use<> + ExactSizeIterator<Item = Port> {
    self.dead_nodes.push(node_id);
    self.nodes[node_id].ports.keys().map(move |index| Port(PortRef { node: node_id, index }))
  }

  pub fn remove_node_n<const N: usize>(&mut self, node_id: NodeId) -> [Port; N] {
    exact_size(self.remove_node(node_id))
  }

  fn tighten_dirty(&mut self) {
    self.maybe_dirty.retain(|&n| {
      if let Some(node) = self.nodes.get_mut(n)
        && node.dirty
      {
        node.dirty = false;
        true
      } else {
        self.not_dirty -= 1;
        false
      }
    });

    for &node in &self.maybe_dirty {
      self.nodes[node].dirty = true;
    }

    assert_eq!(self.not_dirty, 0);
  }

  fn tighten_net(&mut self, net_id: NetId) {
    let net = &mut self.nets[net_id];

    net.maybe_nodes.retain(|&node| {
      if let Some(node) = self.nodes.get_mut(node)
        && node.net == net_id
      {
        node.net = NetId(usize::MAX);
        true
      } else {
        net.not_nodes -= 1;
        false
      }
    });

    for node in &net.maybe_nodes {
      self.nodes[*node].net = net_id;
    }

    assert_eq!(net.not_nodes, 0);
  }

  pub fn finish_edit(&mut self) {
    let mut dead_nodes = take(&mut self.dead_nodes);
    for node in dead_nodes.drain(..) {
      let node = self.nodes.remove(node);
      for port in node.ports.into_values() {
        assert!(port.is_none());
        self.holes -= 1;
      }
      let net = &mut self.nets[node.net];
      net.not_nodes += 1;
      let tighten_net = net.not_nodes * 2 >= net.maybe_nodes.capacity();
      if node.dirty {
        self.not_dirty += 1;
        net.pending -= 1;
        if net.pending == 0 {
          self.finish_edit_net(node.net);
        }
        if self.not_dirty * 2 >= self.maybe_dirty.capacity() {
          self.tighten_dirty();
        }
      }
      if tighten_net {
        self.tighten_net(node.net);
      }
    }
    self.dead_nodes = dead_nodes;
    assert_eq!(self.holes, 0);
  }

  fn finish_edit_net(&mut self, net_id: NetId) {
    let mut awaiting = take(&mut self.nets[net_id].awaiting);
    for (node_id, net_id) in awaiting.drain(..) {
      if self.nodes.get(node_id).is_some_and(|node| node.net == net_id) {
        self.mark_dirty(node_id);
      }
      let net = &mut self.nets[net_id];
      net.pending -= 1;
      if net.pending == 0 {
        self.finish_edit_net(net_id);
      }
    }
    self.nets[net_id].awaiting = awaiting;
  }

  pub fn get_net(&self, name: NameId) -> Option<NetId> {
    self.net_lookup.get(&name).copied()
  }

  pub fn await_net(&mut self, net: NetId, node: NodeId) -> bool {
    let net = &mut self.nets[net];
    if net.pending == 0 {
      return true;
    }
    let net_id = self.nodes[node].net;
    net.awaiting.push((node, net_id));
    self.nets[net_id].pending += 1;
    false
  }

  pub fn graft_net_n<const N: usize>(&mut self, from_id: NetId, into_id: NetId) -> [Port; N] {
    exact_size(self.graft_net(from_id, into_id))
  }

  pub fn graft_net(
    &mut self,
    from_id: NetId,
    into_id: NetId,
  ) -> impl use<> + ExactSizeIterator<Item = Port> {
    assert!(from_id != into_id);

    self.finish_edit();
    // This is needed for correctness so that all nodes in `from.maybe_nodes`
    // actually belong to the from net.
    self.tighten_net(from_id);

    let (from, into) = self.nets.get2_mut(from_id, into_id).unwrap();
    assert_eq!(from.pending, 0);
    for &old_id in &from.maybe_nodes {
      let old = &self.nodes[old_id];
      let dirty = old.dirty;
      let new = Node {
        net: into_id,
        name: old.name.clone(),
        dirty,
        ports: old.ports.values().map(|x| Some(x.as_ref().unwrap()._clone())).collect(),
      };
      let new_id = self.nodes.insert(new);
      into.maybe_nodes.push(new_id);
      if dirty {
        self.maybe_dirty.push(new_id);
      }
      // We exploit the net field to store the mapping from old nodes to their new
      // clone. We can later restore the field since we know that all old nodes belong
      // to the from net.
      self.nodes[old_id].net.0 = new_id.0; // crimes
    }

    let map_id = |nodes: &IdxSlab<NodeId, Node>, old_id| NodeId(nodes[old_id].net.0); // crimes

    let free = map_id(&self.nodes, from.free);

    for &old_id in &from.maybe_nodes {
      let new_id = map_id(&self.nodes, old_id);
      let mut ports = take(&mut self.nodes[new_id].ports);
      for port in ports.values_mut().filter_map(|x| x.as_mut()) {
        port.0.node = map_id(&self.nodes, port.0.node);
      }
      self.nodes[new_id].ports = ports;
    }

    for &old_id in &from.maybe_nodes {
      self.nodes[old_id].net = from_id; // hide the evidence
    }

    let mut ports = self.remove_node(free);
    self.link(ports.next().unwrap(), ports.next().unwrap());
    ports
  }

  fn run(&mut self, table: &mut Table, rules: &Rules, limit: &mut Option<usize>) {
    let mut steps = 0;
    while limit.is_none_or(|n| steps < n)
      && let Some(node_id) = self.maybe_dirty.pop()
    {
      if let Some(node) = self.nodes.get_mut(node_id)
        && node.dirty
      {
        let net_id = node.net;
        node.dirty = false;
        if self.visit(table, rules, net_id, node_id) {
          steps += 1;
        }
        self.finish_edit();
        let net = &mut self.nets[net_id];
        net.pending -= 1;
        if net.pending == 0 {
          self.finish_edit_net(net_id);
        }
      } else {
        self.not_dirty -= 1;
      }
    }
    if let Some(limit) = limit {
      *limit -= steps;
    }
  }

  fn visit(&mut self, table: &mut Table, rules: &Rules, net: NetId, node: NodeId) -> bool {
    let path = self.name(node).path;
    let pri_other = self.follow(node.port(0));
    if pri_other.is_principal() {
      let other = pri_other.node;
      let other_path = self.name(other).path;
      if let Some(rules) = rules.interaction.get(&(path, other_path)) {
        for (flip, f) in rules {
          let (a, b) = flip_if(*flip, (node, other));
          if f(self, table, net, a, b) {
            return true;
          }
        }
      }
    }
    if let Some(rules) = rules.rewrite.get(&path) {
      for f in rules {
        if f(self, table, net, node) {
          return true;
        }
      }
    }
    false
  }

  fn add_flat_nets(
    &mut self,
    table: &mut Table,
    nets: &HashMap<NameId, FlatNet>,
    filter: impl Fn(NameId) -> bool,
  ) {
    let mut names = nets.keys().copied().filter(|&n| filter(n)).collect::<Vec<_>>();
    names.sort();
    for name in names {
      self.add_flat_net(table, name, &nets[&name]);
    }
  }

  pub fn add_flat_net(&mut self, table: &mut Table, name: NameId, net: &FlatNet) {
    assert_eq!(self.holes, 0);

    let free = table.add_path("ivy:engine:free");

    let net_id = self.nets.push(Net {
      name,
      free: NodeId(usize::MAX),
      maybe_nodes: vec![],
      not_nodes: 0,
      pending: 0,
      awaiting: vec![],
    });

    let mut wires = HashMap::<Wire, Port>::new();

    let mut link =
      |self_: &mut Self, wire: Wire, port| match wires.entry(net.links.canonicalize_ref(wire)) {
        Entry::Vacant(e) => {
          e.insert(port);
        }
        Entry::Occupied(e) => {
          self_.link(port, e.remove());
        }
      };

    // We add two extra ports as we don't want to connect the principal port of the
    // free port node to any of the free ports as we never want it to participate in
    // an interaction rewrite. Since we can't leave the principal port disconnected,
    // we link it to the first aux port.
    let (free_id, mut free_ports) = self._insert_node(net_id, free.into(), 2 + net.free.len());
    self.link(free_ports.next().unwrap(), free_ports.next().unwrap());
    self.nets[net_id].free = free_id;

    for (&wire, port) in net.free.iter().zip(free_ports) {
      link(self, wire, port);
    }

    for node in &net.nodes {
      let ports = self.insert_node(net_id, node.name.clone(), 1 + node.aux.len());
      let wires = iter::once(node.pri).chain(node.aux.iter().copied());
      for (wire, port) in wires.zip(ports) {
        link(self, wire, port);
      }
    }

    self.net_lookup.insert(name, net_id);

    assert_eq!(self.holes, 0);
  }

  fn finish(mut self, nets: &mut HashMap<NameId, FlatNet>) {
    let mut wires = HashMap::new();
    for (net_id, info) in take(&mut self.nets) {
      let mut net = FlatNet::default();

      for node_id in info.maybe_nodes {
        if self.nodes.get(node_id).is_some_and(|node| node.net == net_id) {
          let node = self.nodes.remove(node_id);
          let mut wires = node.ports.into_iter().map(|(index, other)| {
            let canonical = node_id.port(index).min(*other.unwrap());
            match wires.entry(canonical) {
              Entry::Occupied(e) => e.remove(),
              Entry::Vacant(e) => *e.insert(net.wire()),
            }
          });
          let pri = wires.next().unwrap();
          let mut aux = wires.collect::<Vec<_>>();
          if node_id == info.free {
            _ = aux.remove(0);
            net.free = aux;
          } else {
            net.nodes.push(FlatNode { name: node.name, pri, aux })
          }
        }
      }
      assert!(wires.is_empty());

      nets.insert(info.name, net);
    }
  }
}

#[allow(clippy::type_complexity)]
#[derive(Default)]
pub struct Rules<'a> {
  interaction: HashMap<
    (PathId, PathId),
    Vec<(
      /* flipped */ bool,
      Rc<dyn 'a + Fn(&mut Engine, &mut Table, NetId, NodeId, NodeId) -> bool /* applied */>,
    )>,
  >,
  rewrite: HashMap<
    PathId,
    Vec<Rc<dyn 'a + Fn(&mut Engine, &mut Table, NetId, NodeId) -> bool /* applied */>>,
  >,
}

impl<'a> Rules<'a> {
  pub fn new(rules: impl Register<Rules<'a>>) -> Self {
    let mut self_ = Self::default();
    self_.register(rules);
    self_
  }

  pub fn register(&mut self, rules: impl Register<Rules<'a>>) {
    rules.register(self);
  }

  pub fn register_interaction(
    &mut self,
    pairs: impl IntoIterator<Item = (PathId, PathId)>,
    f: impl 'a + Fn(&mut Engine, &mut Table, NetId, NodeId, NodeId) -> bool,
  ) {
    let f = Rc::new(f);
    for pair in pairs {
      self.interaction.entry(pair).or_default().push((false, f.clone()));
      if pair != flip(pair) {
        self.interaction.entry(flip(pair)).or_default().push((true, f.clone()));
      }
    }
  }

  pub fn register_rewrite(
    &mut self,
    paths: impl IntoIterator<Item = PathId>,
    f: impl 'a + Fn(&mut Engine, &mut Table, NetId, NodeId) -> bool,
  ) {
    let f = Rc::new(f);
    for path in paths {
      self.rewrite.entry(path).or_default().push(f.clone());
    }
  }
}

pub struct Interaction<
  I: IntoIterator<Item = (PathId, PathId)>,
  F: Fn(&mut Engine, &mut Table, NetId, NodeId, NodeId) -> bool,
>(pub I, pub F);

impl<
  'a,
  I: IntoIterator<Item = (PathId, PathId)>,
  F: 'a + Fn(&mut Engine, &mut Table, NetId, NodeId, NodeId) -> bool,
> Register<Rules<'a>> for Interaction<I, F>
{
  fn register(self, rules: &mut Rules<'a>) {
    let Interaction(paths, f) = self;
    rules.register_interaction(paths, f);
  }
}

pub struct Rewrite<
  I: IntoIterator<Item = PathId>,
  F: Fn(&mut Engine, &mut Table, NetId, NodeId) -> bool,
>(pub I, pub F);

impl<'a, I: IntoIterator<Item = PathId>, F: 'a + Fn(&mut Engine, &mut Table, NetId, NodeId) -> bool>
  Register<Rules<'a>> for Rewrite<I, F>
{
  fn register(self, rules: &mut Rules<'a>) {
    let Rewrite(paths, f) = self;
    rules.register_rewrite(paths, f);
  }
}

fn flip<T>((a, b): (T, T)) -> (T, T) {
  (b, a)
}

fn flip_if<T>(cond: bool, pair: (T, T)) -> (T, T) {
  if cond { flip(pair) } else { pair }
}
