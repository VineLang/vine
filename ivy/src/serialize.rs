use std::{
  cell::UnsafeCell,
  collections::{BTreeMap, HashMap},
  marker::PhantomData,
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
use vine_util::bicycle::{Bicycle, BicycleState};

use crate::ast::{Net, Nets, Tree};

#[derive(Debug, Default, Clone)]
pub struct SerializeOptions {
  pub black_box: bool,
}

impl Nets {
  pub fn serialize<'ivm>(
    &self,
    globals: &'ivm mut Vec<Global<'ivm>>,
    opts: SerializeOptions,
  ) -> &'ivm [Global<'ivm>] {
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
      opts,
      current: Default::default(),
      equivalences: Default::default(),
      registers: Default::default(),
      labels: Default::default(),
    };

    for (i, net) in self.values().enumerate() {
      serializer.current = take(unsafe { &mut *globals[i].get() });
      serializer.serialize_net(net);
      unsafe { *globals[i].get() = take(&mut serializer.current) };
    }

    PropagateLabels(PhantomData).visit_all(globals);

    unsafe { transmute::<&[UnsafeCell<Global<'ivm>>], &[Global<'ivm>]>(globals) }
  }
}

pub struct Serializer<'ast, 'ivm> {
  globals: &'ivm [UnsafeCell<Global<'ivm>>],
  nets: &'ast Nets,
  opts: SerializeOptions,
  current: Global<'ivm>,
  equivalences: BTreeMap<&'ast str, &'ast str>,
  registers: HashMap<&'ast str, Register>,
  labels: IndexSet<&'ast str>,
}

impl<'ast, 'ivm> Serializer<'ast, 'ivm> {
  fn push(&mut self, instruction: Instruction<'ivm>) {
    unsafe { self.current.instructions.push(instruction) }
  }

  fn serialize_net(&mut self, net: &'ast Net) {
    self.equivalences.clear();
    self.registers.clear();

    for (a, b) in &net.pairs {
      let a = if self.opts.black_box { a } else { a.white_box() };
      let b = if self.opts.black_box { b } else { b.white_box() };
      let (Tree::Var(a), Tree::Var(b)) = (a, b) else { continue };
      let a = self.equivalences.remove(&**a).unwrap_or(a);
      let b = self.equivalences.remove(&**b).unwrap_or(b);
      self.equivalences.insert(a, b);
      self.equivalences.insert(b, a);
    }

    for (a, b) in &self.equivalences {
      if a < b {
        let r = self.current.instructions.new_register();
        self.registers.insert(a, r);
        self.registers.insert(b, r);
      }
    }

    self.serialize_tree_to(&net.root, Register::ROOT);
    for (a, b) in net.pairs.iter().rev() {
      self.serialize_pair(a, b);
    }
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
    let tree = if self.opts.black_box { tree } else { tree.white_box() };
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
      Tree::F32(num) => {
        self.push(Instruction::Nilary(to, Port::new_ext_val(ExtVal::new_f32(*num))))
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
      Tree::Var(v) => {
        let old = self.registers.insert(v, to);
        debug_assert!(old.is_none());
      }
      Tree::BlackBox(x) => {
        if self.opts.black_box {
          let x = self.serialize_tree(x);
          self.push(Instruction::InertPair(to, x));
        } else {
          self.serialize_tree_to(x, to);
        }
      }
    }
  }
}

struct PropagateLabels<'ivm>(PhantomData<&'ivm mut &'ivm ()>);

impl<'ivm> Bicycle for PropagateLabels<'ivm> {
  type Node = &'ivm UnsafeCell<Global<'ivm>>;

  fn state(&mut self, cur: Self::Node) -> &BicycleState {
    unsafe { &*(&mut (*cur.get()).flag as *mut usize as *const BicycleState) }
  }

  fn visit(&mut self, cur: Self::Node, mut recurse: impl FnMut(&mut Self, Self::Node)) {
    let instructions = unsafe { (*cur.get()).instructions.instructions() };

    for i in instructions {
      match i {
        Instruction::Nilary(_, p) if p.tag() == Tag::Global => {
          let child = unsafe { &*p.addr().0.cast::<UnsafeCell<Global>>() };
          if !ptr::addr_eq(child, cur) {
            recurse(self, child);
            unsafe { (*cur.get()).labels.union(&(*child.get()).labels) }
          }
        }
        _ => {}
      }
    }
  }
}
