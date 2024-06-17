use std::{
  cell::UnsafeCell,
  collections::{btree_map::Entry, BTreeMap, HashMap},
  mem::{take, transmute},
  ptr,
};

use indexmap::IndexSet;
use ivm::{
  addr::Addr,
  ext::ExtVal,
  global::{Global, LabelSet},
  instruction::{Instruction, Instructions, Register},
  port::{Port, Tag},
};

use crate::ast::{Net, Nets, Tree};

impl Nets {
  pub fn serialize<'ivm>(&self, globals: &'ivm mut Vec<Global<'ivm>>) -> &'ivm [Global<'ivm>] {
    assert!(globals.is_empty());
    globals.extend(self.keys().map(|name| Global {
      name: name.clone(),
      labels: LabelSet::NONE,
      instructions: Instructions::default(),
      flag: 0,
    }));
    let globals = &mut globals[..];
    let globals = unsafe { transmute::<&mut [Global<'ivm>], &[UnsafeCell<Global<'ivm>>]>(globals) };

    let mut serializer = Serializer {
      globals,
      nets: self,
      current: Default::default(),
      chains: Default::default(),
      registers: Default::default(),
      labels: Default::default(),
    };

    serializer.labels.insert("lam");

    for (i, net) in self.values().enumerate() {
      serializer.current = take(unsafe { &mut *globals[i].get() });
      serializer.serialize_net(net);
      unsafe { *globals[i].get() = take(&mut serializer.current) };
    }

    for cur in globals {
      serializer.visit(cur, Some(1));
    }

    unsafe { transmute::<&[UnsafeCell<Global<'ivm>>], &[Global<'ivm>]>(globals) }
  }
}

pub struct Serializer<'ast, 'ivm> {
  globals: &'ivm [UnsafeCell<Global<'ivm>>],
  nets: &'ast Nets,
  current: Global<'ivm>,
  chains: BTreeMap<&'ast str, &'ast str>,
  registers: HashMap<&'ast str, Register>,
  labels: IndexSet<&'ast str>,
}

impl<'ast, 'ivm> Serializer<'ast, 'ivm> {
  fn push(&mut self, instruction: Instruction<'ivm>) {
    unsafe { self.current.instructions.push(instruction) }
  }

  fn serialize_net(&mut self, net: &'ast Net) {
    self.chains.clear();
    self.registers.clear();

    for (a, b) in &net.pairs {
      let (Tree::Var(a), Tree::Var(b)) = (a, b) else { continue };
      let a = self.follow_chain(a);
      let b = self.follow_chain(b);
      self.chains.insert(a, b);
      self.chains.insert(b, a);
    }

    for (a, b) in &self.chains {
      if a < b {
        let r = self.current.instructions.new_register();
        self.registers.insert(a, r);
        self.registers.insert(b, r);
      }
    }

    self.serialize_tree_to(&net.root, Register::ROOT);
    for (a, b) in &net.pairs {
      self.serialize_pair(a, b);
    }
  }

  fn follow_chain(&mut self, mut x: &'ast str) -> &'ast str {
    if let Entry::Occupied(e) = self.chains.entry(x) {
      x = e.remove();
      self.chains.remove(x);
    }
    x
  }

  fn serialize_pair(&mut self, a: &'ast Tree, b: &'ast Tree) {
    let (a, b) = match (a, b) {
      (Tree::Var(_), Tree::Var(_)) => return,
      (a, b @ Tree::Var(_)) => (b, a),
      (a, b) => (a, b),
    };
    let to = self.serialize_tree(a);
    self.serialize_tree_to(b, to);
  }

  fn serialize_tree(&mut self, tree: &'ast Tree) -> Register {
    if let Tree::Var(var) = tree {
      *self.registers.entry(var).or_insert_with(|| self.current.instructions.new_register())
    } else {
      let r = self.current.instructions.new_register();
      self.serialize_tree_to(tree, r);
      r
    }
  }

  fn serialize_tree_to(&mut self, tree: &'ast Tree, to: Register) {
    match tree {
      Tree::Erase => self.push(Instruction::Nilary(to, Port::ERASE)),
      Tree::U32(num) => {
        self.push(Instruction::Nilary(to, Port::new_ext_val(ExtVal::new_u32(*num))))
      }
      Tree::Comb(label, a, b) => {
        let label = self.labels.insert_full(label).0 as u16;
        self.current.labels.add(label);
        let a = self.serialize_tree(a);
        let b = self.serialize_tree(b);
        self.push(Instruction::Binary(Tag::Comb, label, to, a, b));
      }
      Tree::ExtFn(f, a, b) => {
        let a = self.serialize_tree(a);
        let b = self.serialize_tree(b);
        self.push(Instruction::Binary(Tag::ExtFn, f.bits(), to, a, b));
      }
      Tree::Global(name) => {
        let r = &self.globals[self.nets.get_index_of(name).expect("undefined global")];
        let port =
          unsafe { Port::new(Tag::Global, 0, Addr(r as *const UnsafeCell<Global> as *const ())) };
        self.push(Instruction::Nilary(to, port));
      }
      Tree::Branch(z, p, o) => {
        let a = self.current.instructions.new_register();
        let z = self.serialize_tree(z);
        let p = self.serialize_tree(p);
        self.push(Instruction::Binary(Tag::Branch, 0, a, z, p));
        let o = self.serialize_tree(o);
        self.push(Instruction::Binary(Tag::Branch, 0, to, a, o));
      }
      Tree::Var(_) => unreachable!(),
    }
  }

  fn visit(&self, cur: &'ivm UnsafeCell<Global<'ivm>>, depth: Option<usize>) -> usize {
    let flag = unsafe { &mut (*cur.get()).flag };
    match *flag {
      0 => *flag = depth.unwrap(),
      usize::MAX => return usize::MAX,
      d if depth.is_some() => return d,
      _ => *flag = usize::MAX,
    };

    let head_depth = self._visit(cur, depth);

    let flag = unsafe { &mut (*cur.get()).flag };
    if depth.is_some_and(|x| x > head_depth) {
      *flag = head_depth;
    } else {
      *flag = usize::MAX;
      if depth == Some(head_depth) {
        self._visit(cur, None);
      }
    }

    head_depth
  }

  fn _visit(&self, cur: &UnsafeCell<Global<'ivm>>, depth: Option<usize>) -> usize {
    let instructions = unsafe { (*cur.get()).instructions.instructions() };

    let mut head_depth = usize::MAX;
    for i in instructions {
      match i {
        Instruction::Nilary(_, p) if p.tag() == Tag::Global => {
          let child = unsafe { &*p.addr().0.cast::<UnsafeCell<Global<'ivm>>>() };
          if !ptr::addr_eq(child, cur) {
            head_depth = head_depth.min(self.visit(child, depth.map(|x| x + 1)));
            unsafe { (*cur.get()).labels.union(&(*child.get()).labels) }
          }
        }
        _ => {}
      }
    }
    head_depth
  }
}
