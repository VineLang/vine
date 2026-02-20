use vine_util::{idx::IdxVec, new_idx};

use crate::name::Name;

pub struct Net {
  free_nodes: Vec<NodeId>,
  nodes: IdxVec<NodeId, Option<Node>>,
  dirty: Vec<NodeId>,
  holes: usize,
  dead_nodes: Vec<NodeId>,
}

pub struct Node {
  name: Name,
  dirty: bool,
  ports: IdxVec<PortIndex, Port>,
}

pub struct Port(NodeId, PortIndex);

impl Port {
  fn clone(&self) -> Self {
    Self(self.0, self.1)
  }
}

new_idx!(pub NodeId);
new_idx!(pub PortIndex);

impl Net {
  fn mark_dirty(&mut self, id: NodeId) {
    let node = self.nodes[id].as_mut().unwrap();
    if !node.dirty {
      node.dirty = true;
      self.dirty.push(id);
    }
  }

  pub fn other_port(&self, p: &Port) -> &Port {
    &self.nodes[p.0].as_ref().unwrap().ports[p.1]
  }

  fn other_port_mut(&mut self, p: &Port) -> &mut Port {
    &mut self.nodes[p.0].as_mut().unwrap().ports[p.1]
  }

  pub fn link(&mut self, a: Port, b: Port) {
    self.holes -= 2;

    let a = self.other_port(&a).clone();
    let b = self.other_port(&b).clone();

    *self.other_port_mut(&a) = b.clone();
    *self.other_port_mut(&b) = b.clone();

    self.mark_dirty(a.0);
    self.mark_dirty(b.0);
  }

  pub fn name(&self, id: NodeId) -> &Name {
    &self.nodes[id].as_ref().unwrap().name
  }

  pub fn remove_node(&mut self, id: NodeId) -> impl Iterator<Item = Port> {
    self.dead_nodes.push(id);
    let ports = &self.nodes[id].as_ref().unwrap().ports;
    self.holes += ports.len();
    ports.keys().map(move |p| Port(id, p))
  }

  pub fn insert_node(&mut self, name: Name, len: usize) -> impl Iterator<Item = Port> {
    self.holes += len;
    let id = self.free_nodes.pop().unwrap_or_else(|| self.nodes.push(None));
    self.dirty.push(id);
    let iter = (0..len).map(move |i| Port(id, PortIndex(i)));
    self.nodes[id] = Some(Node { name, dirty: true, ports: iter.clone().collect() });
    iter
  }

  pub fn run(&mut self, reducer: &mut impl Reducer) {
    while let Some(id) = self.dirty.pop() {
      if let Some(node) = &self.nodes[id]
        && node.dirty
      {
        reducer.visit(self, id);
        assert!(self.holes == 0);
        self.free_nodes.append(&mut self.dead_nodes);
      }
    }
  }
}

pub trait Reducer {
  fn visit(&mut self, net: &mut Net, node: NodeId);
}
