use std::{
  collections::{hash_map::Entry, HashMap, HashSet},
  mem::take,
};

use crate::ast::{Net, Nets, Tree};

impl Nets {
  pub fn pre_reduce(&mut self) {
    let mut pre_reduce = PreReduce {
      nets: self,
      done: Vec::new(),
      pairs: Vec::new(),
      vars: HashMap::new(),
      frozen: HashSet::new(),
      next_var: 0,
    };
    let mut new_nets = Nets::default();
    for (name, net) in self.iter() {
      new_nets.insert(name.clone(), pre_reduce.reduce(net));
    }
    *self = new_nets;
  }
}

#[derive(Debug)]
struct PreReduce<'n> {
  nets: &'n Nets,
  done: Vec<(Tree, Tree)>,
  pairs: Vec<(Tree, Tree)>,
  vars: HashMap<String, Tree>,
  frozen: HashSet<String>,
  next_var: usize,
}

macro_rules! sym {
  ($a:pat, $b:pat) => {
    ($a, $b) | ($b, $a)
  };
}

impl PreReduce<'_> {
  fn reduce(&mut self, net: &Net) -> Net {
    self.next_var = 0;
    let vars = &mut HashMap::new();
    let root = self.alpha(&net.root, vars);
    self.freeze(&root);
    for (a, b) in net.pairs.iter().rev() {
      let a = self.alpha(a, vars);
      let b = self.alpha(b, vars);
      self.pairs.push((a, b));
    }
    self._reduce();
    assert!(self.frozen.is_empty());
    assert!(self.vars.is_empty());
    assert!(self.pairs.is_empty());
    Net { root, pairs: take(&mut self.done) }
  }

  fn _reduce(&mut self) {
    while let Some((a, b)) = self.pairs.pop() {
      match (a, b) {
        sym!(a @ Tree::BlackBox(_), b) => {
          let i = self.reserve_done();
          self.freeze(&a);
          self.freeze(&b);
          self.done[i] = (a, b);
        }
        sym!(Tree::Var(v), t) => {
          if self.frozen.remove(&v) {
            let i = self.reserve_done();
            self.freeze(&t);
            self.done[i] = (Tree::Var(v), t);
          } else {
            match self.vars.entry(v) {
              Entry::Vacant(e) => {
                e.insert(t);
              }
              Entry::Occupied(e) => self.pairs.push((t, e.remove())),
            }
          }
        }
        sym!(Tree::Erase, mut x) => {
          for x in x.children_mut() {
            self.pairs.push((Tree::Erase, take(x)));
          }
        }
        sym!(Tree::Global(g), t) => {
          let vars = &mut HashMap::new();
          let net = &self.nets[&g];
          let root = self.alpha(&net.root, vars);
          self.pairs.push((t, root));
          for (a, b) in net.pairs.iter().rev() {
            let a = self.alpha(a, vars);
            let b = self.alpha(b, vars);
            self.pairs.push((a, b));
          }
          assert!(vars.is_empty());
        }
        (Tree::Comb(x, a, b), Tree::Comb(y, c, d)) if x == y => {
          self.pairs.push((*a, *c));
          self.pairs.push((*b, *d));
        }
        // sym!((Global, c), (Comb, d)) if !unsafe { c.as_global() }.labels.has(d.label()) => {
        //   self.copy(c, d)
        // }
        // sym!((Global, c), (_, p)) => self.expand(c, p),
        // ((Comb, a), (Comb, b)) | ((ExtFn, a), (ExtFn, b)) | ((Branch, a), (Branch, b))
        //   if a.label() == b.label() =>
        // {
        //   self.annihilate(a, b)
        // }
        // sym!((Erase, n), (_, b)) | sym!((ExtVal, n), (Comb, b)) => self.copy(n, b),
        // ((Comb | ExtFn | Branch, a), (Comb | ExtFn | Branch, b)) => self.commute(a, b),
        // sym!((ExtFn, f), (ExtVal, v)) => self.call(f, v),
        // sym!((Branch, b), (ExtVal, v)) => self.branch(b, v),
        (a, b) => {
          let i = self.reserve_done();
          self.freeze(&a);
          self.freeze(&b);
          self.done[i] = (a, b);
        }
      }
    }
  }

  fn freeze(&mut self, tree: &Tree) {
    if let Tree::Var(v) = tree {
      if let Some(t) = self.vars.remove(v) {
        let i = self.reserve_done();
        self.freeze(&t);
        self.done[i] = (Tree::Var(v.clone()), t);
      } else {
        let inserted = self.frozen.insert(v.clone());
        if !inserted {
          self.frozen.remove(v);
        }
      }
    } else {
      for child in tree.children() {
        self.freeze(child)
      }
    }
  }

  fn alpha(&mut self, tree: &Tree, vars: &mut HashMap<String, String>) -> Tree {
    let mut tree = tree.clone();
    self._alpha(&mut tree, vars);
    tree
  }

  fn _alpha(&mut self, tree: &mut Tree, vars: &mut HashMap<String, String>) {
    if let Tree::Var(v) = tree {
      match vars.entry(take(v)) {
        Entry::Occupied(e) => *v = e.remove(),
        Entry::Vacant(e) => {
          *v = self.new_var();
          e.insert(v.clone());
        }
      }
    } else {
      for t in tree.children_mut() {
        self._alpha(t, vars);
      }
    }
  }

  fn new_var(&mut self) -> String {
    let i = self.next_var;
    self.next_var += 1;
    format!("v{i}")
  }

  fn reserve_done(&mut self) -> usize {
    let i = self.done.len();
    self.done.push(Default::default());
    i
  }
}
