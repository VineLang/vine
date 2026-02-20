use std::{
  collections::{HashMap, hash_map::Entry},
  mem::take,
};

use vine_util::{idx::Counter, new_idx};

use crate::name::{Name, NameId};

#[derive(Debug, Clone)]
pub struct FlatNet<N = FlatNode> {
  pub wires: Counter<Wire>,
  pub free: Vec<Wire>,
  pub nodes: Vec<N>,
  pub links: WireLinks,
}

#[derive(Debug, Clone)]
pub struct FlatNode {
  pub name: Name,
  pub pri: Wire,
  pub aux: Vec<Wire>,
}

#[allow(non_snake_case)]
pub fn FlatNode(name: impl Into<Name>, pri: Wire, aux: impl IntoIterator<Item = Wire>) -> FlatNode {
  FlatNode { name: name.into(), pri, aux: aux.into_iter().collect() }
}

pub trait FlatDecode<Ctx>: Sized {
  type Error;
  fn decode(node: FlatNode, ctx: &mut Ctx) -> Result<Self, Self::Error>;
}

pub trait FlatEncode<Ctx>: Sized {
  fn encode(node: Self, ctx: &mut Ctx) -> FlatNode;
}

impl<N> FlatNet<N> {
  pub fn encode<Ctx>(self, ctx: &mut Ctx) -> FlatNet
  where
    N: FlatEncode<Ctx>,
  {
    FlatNet {
      wires: self.wires,
      free: self.free,
      nodes: self.nodes.into_iter().map(|node| N::encode(node, ctx)).collect(),
      links: self.links,
    }
  }

  pub fn wire(&mut self) -> Wire {
    self.wires.next()
  }

  pub fn wires<const L: usize>(&mut self) -> [Wire; L] {
    [(); L].map(|_| self.wires.next())
  }

  pub fn link(&mut self, a: Wire, b: Wire) {
    self.links.link(a, b)
  }

  pub fn push(&mut self, node: impl Into<N>) {
    self.nodes.push(node.into())
  }

  pub fn add(&mut self, name: impl Into<Name>, pri: Wire, aux: impl IntoIterator<Item = Wire>)
  where
    FlatNode: Into<N>,
  {
    self.push(FlatNode(name, pri, aux));
  }

  #[must_use]
  pub fn make(&mut self, name: impl Into<Name>, aux: impl IntoIterator<Item = Wire>) -> Wire
  where
    FlatNode: Into<N>,
  {
    let pri = self.wire();
    self.add(name, pri, aux);
    pri
  }
}

impl FlatNet {
  pub fn decode<N: FlatDecode<Ctx>, Ctx>(self, ctx: &mut Ctx) -> Result<FlatNet<N>, N::Error> {
    Ok(FlatNet {
      wires: self.wires,
      free: self.free,
      nodes: self.nodes.into_iter().map(|node| N::decode(node, ctx)).collect::<Result<_, _>>()?,
      links: self.links,
    })
  }

  pub fn resolve_links(&mut self) {
    if !self.links.links.is_empty() {
      for wire in &mut self.free {
        *wire = self.links.canonicalize(*wire);
      }
      for node in &mut self.nodes {
        node.pri = self.links.canonicalize(node.pri);
        for wire in &mut node.aux {
          *wire = self.links.canonicalize(*wire);
        }
      }
    }
  }

  pub fn to_tree(mut self) -> TreeNet {
    self.resolve_links();
    TreeNet {
      wires: self.wires,
      free: self.free.into_iter().map(TreeNode::Wire).collect(),
      links: self
        .nodes
        .into_iter()
        .map(|node| {
          (
            TreeNode::Wire(node.pri),
            TreeNode::Node(node.name, node.aux.into_iter().map(TreeNode::Wire).collect()),
          )
        })
        .collect(),
    }
  }

  pub fn from_tree(net: TreeNet) -> FlatNet {
    net.to_flat()
  }

  pub fn from_tree_nets(nets: HashMap<NameId, TreeNet>) -> HashMap<NameId, FlatNet> {
    nets.into_iter().map(|(name, net)| (name, net.to_flat())).collect()
  }
}

#[derive(Debug, Clone)]
pub struct TreeNet<N = TreeNode> {
  pub wires: Counter<Wire>,
  pub free: Vec<N>,
  pub links: Vec<(N, N)>,
}

#[derive(Debug, Clone)]
pub enum TreeNode {
  Wire(Wire),
  Node(Name, Vec<TreeNode>),
}

pub trait TreeDecode<Ctx>: Sized {
  type Error;
  fn decode(node: TreeNode, ctx: &mut Ctx) -> Result<Self, Self::Error>;
}

pub trait TreeEncode<Ctx>: Sized {
  fn encode(node: Self, ctx: &mut Ctx) -> TreeNode;
}

impl<N> TreeNet<N> {
  pub fn encode<Ctx>(self, ctx: &mut Ctx) -> TreeNet
  where
    N: TreeEncode<Ctx>,
  {
    TreeNet {
      wires: self.wires,
      free: self.free.into_iter().map(|node| N::encode(node, ctx)).collect(),
      links: self.links.into_iter().map(|(a, b)| (N::encode(a, ctx), N::encode(b, ctx))).collect(),
    }
  }
}

impl TreeNet {
  pub fn decode<N: TreeDecode<Ctx>, Ctx>(self, ctx: &mut Ctx) -> Result<TreeNet<N>, N::Error> {
    Ok(TreeNet {
      wires: self.wires,
      free: self.free.into_iter().map(|node| N::decode(node, ctx)).collect::<Result<_, _>>()?,
      links: self
        .links
        .into_iter()
        .map(|(a, b)| Ok((N::decode(a, ctx)?, N::decode(b, ctx)?)))
        .collect::<Result<_, _>>()?,
    })
  }

  pub fn resolve_links(&mut self) {
    let mut wire_links = WireLinks::default();
    for (a, b) in &self.links {
      if let (&TreeNode::Wire(a), &TreeNode::Wire(b)) = (a, b) {
        wire_links.link(a, b);
      }
    }

    let mut trees = HashMap::new();
    for (a, b) in take(&mut self.links) {
      match (a, b) {
        (TreeNode::Wire(_), TreeNode::Wire(_)) => {}
        (TreeNode::Wire(wire), tree) | (tree, TreeNode::Wire(wire)) => {
          let wire = wire_links.canonicalize(wire);
          match trees.entry(wire) {
            Entry::Vacant(e) => {
              e.insert(tree);
            }
            Entry::Occupied(e) => {
              self.links.push((e.remove(), tree));
            }
          }
        }
        (a, b) => {
          self.links.push((a, b));
        }
      }
    }

    fn visit(trees: &mut HashMap<Wire, TreeNode>, tree: &mut TreeNode) {
      match tree {
        TreeNode::Wire(wire) => {
          if let Some(extracted) = trees.remove(wire) {
            *tree = extracted;
            visit(trees, tree);
          }
        }
        TreeNode::Node(_, children) => {
          for child in children {
            visit(trees, child);
          }
        }
      }
    }

    if !trees.is_empty() {
      for free in &mut self.free {
        visit(&mut trees, free);
      }
      for (a, b) in &mut self.links {
        visit(&mut trees, a);
        visit(&mut trees, b);
      }
    }

    if !trees.is_empty() {
      // the net has vicious circles
      let base = self.links.len();
      for (wire, tree) in trees {
        self.links.push((TreeNode::Wire(wire), tree));
      }
      // sort to cure HashMap non-determinism
      self.links[base..]
        .sort_by_key(|x| if let TreeNode::Wire(wire) = &x.0 { wire.0 } else { unreachable!() });
    }
  }

  pub fn to_flat(self) -> FlatNet {
    let mut net = FlatNet {
      wires: self.wires,
      free: Vec::new(),
      nodes: Vec::new(),
      links: WireLinks::default(),
    };

    fn embed(net: &mut FlatNet, up: Option<Wire>, node: TreeNode) -> Wire {
      match node {
        TreeNode::Wire(wire) => {
          if let Some(up) = up {
            net.link(up, wire);
          }
          wire
        }
        TreeNode::Node(name, children) => {
          let node_index = net.nodes.len();
          let pri = up.unwrap_or_else(|| net.wire());
          net.push(FlatNode { name, pri, aux: vec![] });
          let aux = children.into_iter().map(|child| embed(net, None, child)).collect();
          net.nodes[node_index].aux = aux;
          pri
        }
      }
    }

    for free in self.free {
      let free = embed(&mut net, None, free);
      net.free.push(free);
    }

    for (a, b) in self.links {
      let a = embed(&mut net, None, a);
      embed(&mut net, Some(a), b);
    }

    net
  }

  pub fn from_flat(net: FlatNet) -> TreeNet {
    net.to_tree()
  }

  pub fn from_flat_nets(nets: HashMap<NameId, FlatNet>) -> HashMap<NameId, TreeNet> {
    nets.into_iter().map(|(name, net)| (name, net.to_tree())).collect()
  }
}

new_idx!(pub Wire);

#[derive(Default, Debug, Clone)]
pub struct WireLinks {
  links: HashMap<Wire, Wire>,
}

impl WireLinks {
  pub fn link(&mut self, a: Wire, b: Wire) {
    let a = self.links.remove(&a).unwrap_or(a);
    let b = self.links.remove(&b).unwrap_or(b);
    if a != b {
      self.links.insert(a, b);
      self.links.insert(b, a);
    }
  }

  pub fn get(&mut self, a: Wire) -> Option<Wire> {
    self.links.get(&a).copied()
  }

  pub fn canonicalize(&mut self, a: Wire) -> Wire {
    if let Some(b) = self.links.remove(&a) { a.min(b) } else { a }
  }
}

impl<N> Default for FlatNet<N> {
  fn default() -> Self {
    Self {
      wires: Default::default(),
      free: Default::default(),
      nodes: Default::default(),
      links: Default::default(),
    }
  }
}

impl<N> Default for TreeNet<N> {
  fn default() -> Self {
    Self { wires: Default::default(), free: Default::default(), links: Default::default() }
  }
}
