use std::{
  collections::{hash_map::Entry, HashMap},
  mem::take,
};

use loaned::{take, LoanedMut};
use slab::Slab;

use ivm::ext::ExtFn;
use ivy::ast::{Net, Tree};
use vine_util::multi_iter;

#[derive(Default, Debug)]
pub struct NetBuilder {
  pub agents: Vec<Agent>,
  wires: Slab<Option<Port>>,
}

impl NetBuilder {
  pub fn new_wire(&mut self) -> (Port, Port) {
    let wire = self.wires.insert(None);
    (Port::Wire(wire), Port::Wire(wire))
  }

  pub fn link(&mut self, a: Port, b: Port) {
    let a = self.follow(a);
    let b = self.follow(b);
    let aw = a.wire();
    let bw = b.wire();
    if let Some(w) = aw {
      self.wires[w] = Some(b);
    }
    if let Some(w) = bw {
      self.wires[w] = Some(a);
    }
  }

  pub fn follow(&mut self, port: Port) -> Port {
    if let Some(wire) = port.wire() {
      if let Some(port) = self.wires[wire].take() {
        self.wires.remove(wire);
        if let Some(wire) = port.wire() {
          self.wires[wire] = None;
        }
        return port;
      }
    }
    port
  }

  pub fn finish(&mut self, root_port: Port) -> Net {
    let mut root = Tree::Erase;
    let mut finish = FinishNet {
      net: self,
      pairs: Default::default(),
      subst: Default::default(),
      var: Default::default(),
    };
    finish.finish(root_port, &mut root);
    debug_assert!(finish.subst.is_empty());
    let pairs = LoanedMut::<Vec<_>>::from({ finish }.pairs);
    Net { root, pairs: take!(pairs) }
  }
}

#[derive(Default, Debug, Clone)]
pub enum Port {
  #[default]
  Erase,
  U32(u32),
  F32(f32),
  Global(String),
  Wire(usize),
}

impl Port {
  pub fn wire(&self) -> Option<usize> {
    match *self {
      Port::Wire(w) => Some(w),
      _ => None,
    }
  }

  pub fn can_copy(&self) -> bool {
    matches!(self, Port::Erase | Port::U32(_) | Port::F32(_))
  }

  pub fn is_erase(&self) -> bool {
    matches!(self, Port::Erase)
  }
}

#[derive(Debug)]
pub enum Agent {
  ExtFn(ExtFn, Port, Port, Port),
  Comb(String, Port, Port, Port),
  Branch(Port, Port, Port, Port),
}

impl Agent {
  pub fn ports(&self) -> impl Iterator<Item = &Port> {
    multi_iter!(Ports { Three, Four });
    match self {
      Agent::ExtFn(_, a, b, c) | Agent::Comb(_, a, b, c) => Ports::Three([a, b, c]),
      Agent::Branch(a, b, c, d) => Ports::Four([a, b, c, d]),
    }
  }
}

struct FinishNet<'a> {
  net: &'a mut NetBuilder,
  pairs: Vec<LoanedMut<'a, (Tree, Tree)>>,
  subst: HashMap<usize, SignedTree<'a>>,
  var: VarGen,
}

#[derive(Debug)]
enum SignedTree<'a> {
  Tree(LoanedMut<'a, Tree>),
  Hole(&'a mut Tree),
}

impl<'a> FinishNet<'a> {
  fn finish(&mut self, root: Port, root_tree: &'a mut Tree) {
    self.link_port(root, SignedTree::Hole(root_tree));

    for agent in take(&mut self.net.agents).into_iter() {
      match agent {
        Agent::ExtFn(f, p0, p1, p2) => {
          let ((t1, t2), t0) = LoanedMut::loan_with(Tree::ExtFn(f, hole(), hole()), |t0, l| {
            let Tree::ExtFn(_, t1, t2) = t0 else { unreachable!() };
            (l.loan_mut(t1), l.loan_mut(t2))
          });
          self.link_port(p0, SignedTree::Tree(t0));
          self.link_port(p1, SignedTree::Hole(t1));
          self.link_port(p2, SignedTree::Hole(t2));
        }
        Agent::Comb(l, p0, p1, p2) => {
          let ((t1, t2), t0) = LoanedMut::loan_with(Tree::Comb(l, hole(), hole()), |t0, l| {
            let Tree::Comb(_, t1, t2) = t0 else { unreachable!() };
            (l.loan_mut(t1), l.loan_mut(t2))
          });
          self.link_port(p0, SignedTree::Tree(t0));
          self.link_port(p1, SignedTree::Hole(t1));
          self.link_port(p2, SignedTree::Hole(t2));
        }
        Agent::Branch(p0, p1, p2, p3) => {
          let ((t1, t2, t3), t0) =
            LoanedMut::loan_with(Tree::Branch(hole(), hole(), hole()), |t0, l| {
              let Tree::Branch(t1, t2, t3) = t0 else { unreachable!() };
              (l.loan_mut(t1), l.loan_mut(t2), l.loan_mut(t3))
            });
          self.link_port(p0, SignedTree::Tree(t0));
          self.link_port(p1, SignedTree::Hole(t1));
          self.link_port(p2, SignedTree::Hole(t2));
          self.link_port(p3, SignedTree::Hole(t3));
        }
      }
    }
  }

  fn link_wire(&mut self, wire: usize, tree: SignedTree<'a>) {
    match self.subst.entry(wire) {
      Entry::Occupied(e) => {
        let other = e.remove();
        self.link(other, tree);
      }
      Entry::Vacant(e) => {
        e.insert(tree);
      }
    }
  }

  fn link_port(&mut self, port: Port, tree: SignedTree<'a>) {
    let port = self.net.follow(port);
    let other = match port {
      Port::Erase => Tree::Erase,
      Port::U32(n) => Tree::U32(n),
      Port::F32(n) => Tree::F32(n),
      Port::Global(g) => Tree::Global(g),
      Port::Wire(w) => return self.link_wire(w, tree),
    };
    self.link(SignedTree::Tree(other.into()), tree);
  }

  fn link(&mut self, a: SignedTree<'a>, b: SignedTree<'a>) {
    match (a, b) {
      (SignedTree::Tree(a), SignedTree::Tree(b)) => {
        self.pairs.push((a, b).into());
      }
      (SignedTree::Tree(p), SignedTree::Hole(n)) | (SignedTree::Hole(n), SignedTree::Tree(p)) => {
        p.place(n);
      }
      (SignedTree::Hole(a), SignedTree::Hole(b)) => {
        let v = self.var.gen();
        *a = v.0;
        *b = v.1;
      }
    }
  }
}

fn hole<T: Default>() -> T {
  T::default()
}

#[derive(Debug, Default)]
struct VarGen(usize);

impl VarGen {
  fn gen(&mut self) -> (Tree, Tree) {
    let n = self.0;
    self.0 += 1;
    let str = format!("n{n}");
    (Tree::Var(str.clone()), Tree::Var(str))
  }
}
