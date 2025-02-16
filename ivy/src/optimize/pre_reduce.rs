use std::{
  collections::{hash_map::Entry, HashMap},
  mem::take,
};

use ivm::ext::{ExtFn, ExtTy, ExtVal};
use slab::Slab;
use vine_util::{idx::Counter, new_idx};

use crate::ast::{Net, Nets, Tree};

pub fn pre_reduce(nets: &mut Nets) {
  let mut new = Nets::default();
  let mut pre_reduce =
    PreReduce { wires: Slab::new(), vars: Counter::default(), nets, pairs: Vec::new() };
  for name in nets.keys() {
    if name.starts_with("::mandelbrot_tga::main") {
      new.insert(name.clone(), nets[name].clone());
      continue;
    }
    pre_reduce.vars.0 .0 = 1;
    pre_reduce.expand_net(name, Node(NodeKind::Var(VarId(0)), vec![]));
    new.insert(
      name.clone(),
      Net { root: Tree::Var("v0".into()), pairs: take(&mut pre_reduce.pairs) },
    );
  }
  *nets = new;
}

struct PreReduce<'ast> {
  wires: Slab<Option<Node<'ast>>>,
  vars: Counter<VarId>,
  nets: &'ast Nets,
  pairs: Vec<(Tree, Tree)>,
}

#[derive(Debug)]
struct Node<'ast>(NodeKind<'ast>, Vec<Node<'ast>>);

#[derive(Debug, Clone, Copy)]
enum NodeKind<'ast> {
  Erase,
  Comb(&'ast str),
  Global(&'ast str),
  ExtVal(ExtVal),
  ExtFn(ExtFn),
  Branch,
  Wire(usize),
  Var(VarId),
}

new_idx!(VarId; n => ["v{n}"]);

impl<'ast> PreReduce<'ast> {
  fn expand_net(&mut self, name: &str, node: Node<'ast>) {
    let net = &self.nets[name];
    let vars = &mut HashMap::new();
    let root = self.import_tree(vars, &net.root);
    self.interact(node, root);
    for (a, b) in &net.pairs {
      let a = self.import_tree(vars, a);
      let b = self.import_tree(vars, b);
      self.interact(a, b);
    }
  }

  fn interact(&mut self, a: Node<'ast>, b: Node<'ast>) {
    macro_rules! sym {
      ($a:pat, $b:pat) => {
        ($a, $b) | ($b, $a)
      };
      ($a:pat) => {
        ($a, $a)
      };
    }
    use NodeKind::*;
    match ((a.0, a), (b.0, b)) {
      sym!((Wire(w), _), (_, n)) => self.link(w, n),
      sym!((Var(_), a), (_, b)) => {
        let pair = (self.export_node(&a), self.export_node(&b));
        self.pairs.push(pair);
      }
      sym!((Erase | ExtVal(_), _)) | sym!((Erase, _), (Global(_), _)) => {}
      sym!((Global(name), _), (_, n)) => self.expand_net(name, n),
      sym!((Comb(x), a), (Comb(y), b)) if x == y => self.annihilate(a, b),
      sym!((ExtFn(f), a), (ExtFn(g), b)) if f == g => self.annihilate(a, b),
      ((Branch, a), (Branch, b)) => self.annihilate(a, b),
      sym!((Erase, a), (_, b))
      | sym!((ExtVal(_), a), (Comb(_), b))
      | ((Comb(_) | ExtFn(_) | Branch, a), (Comb(_) | ExtFn(_) | Branch, b)) => self.commute(a, b),
      sym!((ExtFn(_), f), (ExtVal(v), _)) => self.call(f, v),
      sym!((Branch, b), (ExtVal(v), _)) => self.branch(b, v),
    }
  }

  fn import_tree(&mut self, vars: &mut HashMap<&'ast str, usize>, tree: &'ast Tree) -> Node<'ast> {
    let kind = match tree {
      Tree::Erase => NodeKind::Erase,
      Tree::Comb(label, ..) => NodeKind::Comb(label),
      Tree::ExtFn(ext_fn, ..) => NodeKind::ExtFn(*ext_fn),
      Tree::Branch(..) => NodeKind::Branch,
      Tree::N32(n) => NodeKind::ExtVal(ExtVal::new_n32(*n)),
      Tree::F32(n) => NodeKind::ExtVal(ExtVal::new_f32(*n)),
      Tree::Var(v) => NodeKind::Wire(match vars.entry(v) {
        Entry::Occupied(e) => e.remove(),
        Entry::Vacant(e) => {
          let w = self.wires.insert(None);
          e.insert(w);
          w
        }
      }),
      Tree::Global(name) => NodeKind::Global(name),
    };
    let children = tree.children().map(|t| self.import_tree(vars, t)).collect();
    Node(kind, children)
  }

  fn export_node(&mut self, node: &Node<'ast>) -> Tree {
    match node.0 {
      NodeKind::Erase => Tree::Erase,
      NodeKind::ExtVal(val) => match val.ty() {
        ExtTy::N32 => Tree::N32(val.as_n32()),
        ExtTy::F32 => Tree::F32(val.as_f32()),
        _ => unreachable!(),
      },
      NodeKind::ExtFn(f) => Tree::ExtFn(
        f,
        Box::new(self.export_node(&node.1[0])),
        Box::new(self.export_node(&node.1[1])),
      ),
      NodeKind::Comb(label) => Tree::Comb(
        label.into(),
        Box::new(self.export_node(&node.1[0])),
        Box::new(self.export_node(&node.1[1])),
      ),
      NodeKind::Global(name) => Tree::Global(name.into()),
      NodeKind::Branch => Tree::Branch(
        Box::new(self.export_node(&node.1[0])),
        Box::new(self.export_node(&node.1[1])),
        Box::new(self.export_node(&node.1[2])),
      ),
      NodeKind::Wire(w) => {
        if let Some(node) = self.wires[w].take() {
          self.wires.remove(w);
          self.export_node(&node)
        } else {
          let v = self.vars.next();
          self.wires[w] = Some(Node(NodeKind::Var(v), vec![]));
          Tree::Var(format!("{v:?}"))
        }
      }
      NodeKind::Var(v) => Tree::Var(format!("{v:?}")),
    }
  }

  fn link(&mut self, w: usize, n: Node<'ast>) {
    if let Some(m) = self.wires[w].take() {
      self.wires.remove(w);
      self.interact(n, m);
    } else {
      self.wires[w] = Some(n);
    }
  }

  fn annihilate(&mut self, a: Node<'ast>, b: Node<'ast>) {
    for (a, b) in a.1.into_iter().zip(b.1) {
      self.interact(a, b);
    }
  }

  fn commute(&mut self, a: Node<'ast>, b: Node<'ast>) {
    let al = a.1.len();
    let bl = a.1.len();
    let vars = (0..al)
      .map(|_| (0..bl).map(|_| self.wires.insert(None)).collect::<Vec<_>>())
      .collect::<Vec<_>>();
    for (i, a) in a.1.into_iter().enumerate() {
      self.interact(a, Node(b.0, (0..bl).map(|j| Node::new_wire(vars[i][j])).collect()));
    }
    for (j, b) in b.1.into_iter().enumerate() {
      self.interact(b, Node(a.0, (0..al).map(|i| Node::new_wire(vars[i][j])).collect()));
    }
  }

  fn branch(&mut self, mut b: Node<'ast>, v: ExtVal) {
    let o = b.1.pop().unwrap();
    let n = b.1.pop().unwrap();
    let z = b.1.pop().unwrap();
    if v.as_n32() == 0 {
      self.interact(n, Node(NodeKind::Erase, vec![]));
      self.interact(z, o);
    } else {
      self.interact(z, Node(NodeKind::Erase, vec![]));
      self.interact(n, o);
    }
  }

  fn call(&mut self, mut f: Node<'ast>, l: ExtVal) {
    let NodeKind::ExtFn(e) = f.0 else { unreachable!() };
    let o = f.1.pop().unwrap();
    let r = f.1.pop().unwrap();
    if let Node(NodeKind::ExtVal(r), _) = r {
      self.interact(Node(NodeKind::ExtVal(e.call(l, r)), vec![]), o);
    } else {
      self.interact(r, Node(NodeKind::ExtFn(e.swap()), vec![Node(NodeKind::ExtVal(l), vec![]), o]));
    }
  }
}

impl<'ast> Node<'ast> {
  fn new_wire(wire: usize) -> Node<'ast> {
    Node(NodeKind::Wire(wire), vec![])
  }
}
