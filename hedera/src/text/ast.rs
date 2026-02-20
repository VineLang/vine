use std::{
  collections::{BTreeMap, HashMap, hash_map::Entry},
  iter,
};

use vine_util::{idx::Counter, lexer::TokenSet};

use crate::{
  name::{Name, NameId},
  net::{FlatNet, FlatNode, TreeNet, TreeNode, Wire},
  text::lexer::Token,
};

pub struct Nets {
  pub nets: Vec<(NameId, Net)>,
}

pub struct Net {
  pub stmts: Vec<Stmt>,
}

pub enum Stmt {
  Expr(Expr),
  Link(Expr, Expr),
}

pub enum Expr {
  Node(Name, Vec<Option<Expr>>),
  Wire(String),
  Free(Option<usize>),
}

#[derive(Debug, Clone)]
pub enum Diag {
  LexError,
  UnexpectedToken { expected: TokenSet<Token>, found: Token },
  InvalidNumber(String),
  MultipleUplinks,
  InvalidUplink,
  MultipleFree,
  MissingFree,
  DisconnectedWire,
}

impl Net {
  pub fn to_flat(self) -> Result<FlatNet, Diag> {
    let mut state = ToFlat::default();

    #[derive(Default)]
    struct ToFlat {
      net: FlatNet,
      free: BTreeMap<usize, Wire>,
      wires: HashMap<String, Wire>,
    }

    impl ToFlat {
      fn stmt(&mut self, stmt: Stmt) -> Result<(), Diag> {
        match stmt {
          Stmt::Link(a, b) => {
            let wire = self.net.wire();
            self.expr(a, wire)?;
            self.expr(b, wire)?;
          }
          Stmt::Expr(Expr::Node(name, children)) => {
            if Self::has_uplink(&children)? {
              Err(Diag::InvalidUplink)?
            }
            let ports = Vec::from_iter(children.iter().map(|_| self.net.wire()));
            self.net.push(FlatNode { name, ports: ports.clone() });
            for (child, wire) in children.into_iter().zip(ports.into_iter().skip(1)) {
              self.expr(child.unwrap(), wire)?;
            }
          }
          Stmt::Expr(Expr::Wire(_)) => Err(Diag::InvalidUplink)?,
          Stmt::Expr(Expr::Free(_)) => Err(Diag::InvalidUplink)?,
        }
        Ok(())
      }

      fn expr(&mut self, expr: Expr, wire: Wire) -> Result<(), Diag> {
        match expr {
          Expr::Node(name, children) => {
            if Self::has_uplink(&children)? {
              let ports = Vec::from_iter(
                children.iter().map(|x| if x.is_some() { self.net.wire() } else { wire }),
              );
              self.net.push(FlatNode { name, ports: ports.clone() });
              for (child, wire) in children.into_iter().zip(ports) {
                if let Some(expr) = child {
                  self.expr(expr, wire)?;
                }
              }
            } else {
              let ports =
                Vec::from_iter(iter::once(wire).chain(children.iter().map(|_| self.net.wire())));
              self.net.push(FlatNode { name, ports: ports.clone() });
              for (child, wire) in children.into_iter().zip(ports.into_iter().skip(1)) {
                self.expr(child.unwrap(), wire)?;
              }
            }
          }
          Expr::Wire(ident) => match self.wires.entry(ident) {
            Entry::Vacant(e) => {
              e.insert(wire);
            }
            Entry::Occupied(e) => {
              let other = e.remove();
              self.net.link(wire, other);
            }
          },
          Expr::Free(n) => {
            if self.free.insert(n.unwrap_or(0), wire).is_none() {
              Err(Diag::MultipleFree)?
            }
          }
        }
        Ok(())
      }

      fn has_uplink(children: &[Option<Expr>]) -> Result<bool, Diag> {
        let mut has_hole = true;
        for child in children {
          if child.is_none() {
            if !has_hole {
              has_hole = true;
            } else {
              Err(Diag::MultipleUplinks)?
            }
          }
        }
        Ok(has_hole)
      }
    }

    for stmt in self.stmts {
      state.stmt(stmt)?;
    }

    for (i, free) in state.free {
      if i != state.net.free.len() {
        Err(Diag::MissingFree)?
      }
      state.net.free.push(free);
    }

    if !state.wires.is_empty() {
      Err(Diag::DisconnectedWire)?
    }

    Ok(state.net)
  }

  pub fn from_flat(mut net: FlatNet) -> Self {
    net.resolve_links();

    let mut vars = Variables::default();

    Net {
      stmts: net
        .nodes
        .into_iter()
        .map(|node| {
          Stmt::Expr(Expr::Node(
            node.name,
            node
              .ports
              .into_iter()
              .map(|wire| {
                Some(if let Some(i) = net.free.iter().position(|&w| w == wire) {
                  Expr::Free((net.free.len() != 1).then_some(i))
                } else {
                  Expr::Wire(vars.assign(wire))
                })
              })
              .collect(),
          ))
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
          Expr::Node(name, children.into_iter().map(|node| Some(transform(vars, node))).collect())
        }
      }
    }

    let mut stmts = Vec::new();

    let multi_free = net.free.len() != 1;
    for (i, node) in net.free.into_iter().enumerate() {
      stmts.push(Stmt::Link(Expr::Free(multi_free.then_some(i)), transform(&mut vars, node)));
    }

    for (a, b) in net.links {
      stmts.push(Stmt::Link(transform(&mut vars, a), transform(&mut vars, b)))
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
