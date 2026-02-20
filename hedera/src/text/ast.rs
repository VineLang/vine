use std::{
  collections::{BTreeMap, HashMap, hash_map::Entry},
  convert::Infallible,
  fmt,
};

use vine_util::{idx::Counter, lexer::TokenSet};

use crate::{
  name::{Name, NameId},
  net::{FlatNet, FlatNode, TreeNet, TreeNode, Wire},
  text::lexer::Token,
};

#[derive(Debug, Clone)]
pub struct Nets<I = Infallible> {
  pub nets: Vec<(NameId, Net<I>)>,
}

#[derive(Debug, Clone)]
pub struct Net<I = Infallible> {
  pub stmts: Vec<Stmt<I>>,
}

pub type Stmt<I = Infallible> = (Expr<I>, Expr<I>);

#[derive(Debug, Clone)]
pub enum Expr<I = Infallible> {
  Node(Name, Vec<Expr<I>>),
  Wire(String),
  Free(Option<usize>),
  Interpolation(I),
  Subnet(Box<Expr<I>>, Vec<Stmt<I>>),
}

#[derive(Debug, Clone)]
pub enum Diag {
  LexError,
  UnexpectedToken { expected: TokenSet<Token>, found: Token },
  InvalidNumber(String),
  MultipleFree(usize),
  MissingFree(usize),
  DisconnectedWire(String),
  InvalidInterpolation,
}

impl Nets {
  pub fn from_flat_nets(flat_nets: HashMap<NameId, FlatNet>) -> Self {
    let mut nets = Nets { nets: Vec::new() };
    for (id, flat_net) in flat_nets {
      nets.nets.push((id, Net::from_flat(flat_net)));
    }
    nets.nets.sort_by_key(|i| i.0);
    nets
  }

  pub fn from_tree_nets(tree_nets: HashMap<NameId, TreeNet>) -> Self {
    let mut nets = Nets { nets: Vec::new() };
    for (id, tree_net) in tree_nets {
      nets.nets.push((id, Net::from_tree(tree_net)));
    }
    nets.nets.sort_by_key(|i| i.0);
    nets
  }
}

impl<I> Net<I> {
  pub fn to_flat_with_interpolations(
    self,
    interpolate: impl FnMut(&mut FlatNet, I, Wire),
  ) -> Result<FlatNet, Diag> {
    let mut state = ToFlat {
      interpolate,
      net: Default::default(),
      free: Default::default(),
      wires: Default::default(),
    };

    struct ToFlat<F> {
      interpolate: F,
      net: FlatNet,
      free: BTreeMap<usize, Wire>,
      wires: HashMap<String, Wire>,
    }

    impl<F> ToFlat<F> {
      fn stmt<I>(&mut self, (a, b): Stmt<I>) -> Result<(), Diag>
      where
        F: FnMut(&mut FlatNet, I, Wire),
      {
        let a = self.expr(a)?;
        let b = self.expr(b)?;
        self.net.link(a, b);
        Ok(())
      }

      fn expr<I>(&mut self, expr: Expr<I>) -> Result<Wire, Diag>
      where
        F: FnMut(&mut FlatNet, I, Wire),
      {
        Ok(match expr {
          Expr::Node(name, children) => {
            let pri = self.net.wire();
            let aux = children.into_iter().map(|c| self.expr(c)).collect::<Result<_, _>>()?;
            self.net.push(FlatNode { name, pri, aux });
            pri
          }
          Expr::Wire(ident) => match self.wires.entry(ident) {
            Entry::Vacant(e) => {
              let wire = self.net.wire();
              e.insert(wire);
              wire
            }
            Entry::Occupied(e) => e.remove(),
          },
          Expr::Free(n) => {
            let wire = self.net.wire();
            let n = n.unwrap_or(0);
            if self.free.insert(n, wire).is_some() {
              Err(Diag::MultipleFree(n))?
            }
            wire
          }
          Expr::Interpolation(i) => {
            let wire = self.net.wire();
            (self.interpolate)(&mut self.net, i, wire);
            wire
          }
          Expr::Subnet(expr, stmts) => {
            let wire = self.expr(*expr)?;
            for stmt in stmts {
              self.stmt(stmt)?;
            }
            wire
          }
        })
      }
    }

    for stmt in self.stmts {
      state.stmt(stmt)?;
    }

    for (i, free) in state.free {
      if i != state.net.free.len() {
        Err(Diag::MissingFree(state.net.free.len()))?
      }
      state.net.free.push(free);
    }

    if !state.wires.is_empty() {
      let wire = state.wires.keys().min().unwrap();
      Err(Diag::DisconnectedWire(wire.clone()))?
    }

    Ok(state.net)
  }
}

impl Net {
  pub fn from_flat(mut net: FlatNet) -> Self {
    net.resolve_links();

    let mut vars = Variables::default();

    let mut from_wire = |wire| {
      if let Some(i) = net.free.iter().position(|&w| w == wire) {
        Expr::Free((net.free.len() != 1).then_some(i))
      } else {
        Expr::Wire(vars.assign(wire))
      }
    };

    Net {
      stmts: net
        .nodes
        .into_iter()
        .map(|node| {
          (
            from_wire(node.pri),
            Expr::Node(node.name, node.aux.into_iter().map(&mut from_wire).collect()),
          )
        })
        .collect(),
    }
  }

  pub fn from_tree(net: TreeNet) -> Self {
    let mut vars = Variables::default();

    fn transform(vars: &mut Variables, node: TreeNode) -> Expr {
      match node {
        TreeNode::Wire(wire) => Expr::Wire(vars.assign(wire)),
        TreeNode::Node(name, children) => {
          Expr::Node(name, children.into_iter().map(|node| transform(vars, node)).collect())
        }
      }
    }

    let mut stmts = Vec::new();

    let multi_free = net.free.len() != 1;
    for (i, node) in net.free.into_iter().enumerate() {
      stmts.push((Expr::Free(multi_free.then_some(i)), transform(&mut vars, node)));
    }

    for (a, b) in net.links {
      stmts.push((transform(&mut vars, a), transform(&mut vars, b)))
    }

    Net { stmts }
  }
}

#[derive(Default)]
struct Variables {
  var: Counter<usize>,
  wires: HashMap<Wire, usize>,
}

impl Variables {
  fn assign(&mut self, wire: Wire) -> String {
    let n = match self.wires.entry(wire) {
      Entry::Vacant(e) => *e.insert(self.var.next()),
      Entry::Occupied(e) => e.remove(),
    };
    format!("{n}")
  }
}

impl fmt::Display for Diag {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Diag::LexError => write!(f, "lexing error"),
      Diag::UnexpectedToken { expected, found } => write!(f, "expected {expected}; found {found}"),
      Diag::InvalidNumber(n) => write!(f, "invalid number `{n}`"),
      Diag::MultipleFree(i) => write!(f, "multiple free ports with index `{i}`"),
      Diag::MissingFree(i) => write!(f, "missing free port with index `{i}`"),
      Diag::DisconnectedWire(wire) => write!(f, "wire `{wire}` is disconnected"),
      Diag::InvalidInterpolation => write!(f, "invalid interpolation"),
    }
  }
}
