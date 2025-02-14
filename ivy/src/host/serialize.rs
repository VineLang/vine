//! Conversion of Ivy [`Net`]s to IVM [`Global`]s.
//!
//! ## Soundness
//!
//! Doing this serialization soundly is a subtle affair; ultimately, we need to
//! create an immutable cyclic data structure in Rust. There are a few immediate
//! problems with this:
//! - Rust data structures are generally acyclic
//! - Creating cyclic structures generally requires mutation
//!
//! The usual solution to this kind of problem would be to use something like
//! `Arc<Mutex<T>>`. This, however, is not possible in this case, as the
//! ultimate data structure needs to be traversable without any locking.
//!
//! We must therefore take matters into our own hands and write `unsafe` code.
//! In particular, we will use *temporary* interior mutability, via careful
//! placement of `UnsafeCell`s, to construct the cyclic structure. When we're
//! done, we will "freeze" the structure by creating an immutable reference to
//! it that lives until the structure is dropped.
//!
//! Doing this soundly is very subtle, and depends on aspects of the Rust
//! aliasing model that as of yet are not specified. The code is, however,
//! compatible with the two current proposed aliasing models -- stacked borrows
//! and tree borrows -- both theoretically (based on my understanding of the two
//! models) and empirically (based on Miri's implementation of them).
//!
//! The soundness of many of the `unsafe` blocks here is trivial; many of them
//! are simple operations on the contents of the `UnsafeCell`, which are safe as
//! long as we only touch one `UnsafeCell` at a time; and since there is no
//! threading involved here, this is easy to guarantee.
//!
//! The subtlety is in ensuring the soundness of the unsafe blocks where we
//! create `&UnsafeCell<Global>` references; we need to ensure there that they
//! are never derived from an `&Global` reference, as that would be UB. Thus,
//! many convenience methods from IVM for dealing with globals are unusable, as
//! they consistently use `&Global`s. Instead, we must use the lower-level
//! raw-pointer APIs. In particular, up until the point of freezing, we treat
//! the address of a [`Tag::Global`] port as an `&UnsafeCell<Global>` if it's
//! one of the globals we're currently creating, rather than the usual
//! `&Global`.

use std::{
  cell::UnsafeCell,
  collections::{BTreeMap, HashMap},
  mem::take,
  ops::Range,
  ptr,
};

use ivm::{
  addr::Addr,
  global::{Global, LabelSet},
  instruction::{Instruction, Instructions, Register},
  port::{Port, Tag},
};
use vine_util::bicycle::{Bicycle, BicycleState};

use crate::{
  ast::{Net, Nets, Tree},
  host::Host,
};

impl<'ivm> Host<'ivm> {
  /// Inserts `nets` into this host, creating an IVM [`Global`] for each net.
  /// Inserted nets will shadow any old nets with the same name.
  pub fn insert_nets(self: &mut &'ivm mut Host<'ivm>, nets: &Nets) {
    let mut globals_vec = Vec::from_iter(nets.keys().map(|name| Global {
      name: name.clone(),
      labels: LabelSet::NONE,
      instructions: Instructions::default(),
      flag: 0,
    }));

    let globals_mut = &mut globals_vec[..] as *mut [Global<'ivm>];

    // Here, we begin the period of temporary interior mutability. Converting a
    // mutable unique reference to an interior-mutable shared reference is a
    // safe operation (cf. `UnsafeCell::from_mut`).
    let globals = unsafe { &*(globals_mut as *const [UnsafeCell<Global<'ivm>>]) };

    // We ensure `globals` lasts for `'ivm` by pushing it into `self.global_store`,
    // which is append-only, and not dropped until after `'ivm`, since the
    // signature requires the existence of an `&'ivm mut Host<'ivm>`.
    self.global_store.push(globals_vec);

    self.globals.extend(nets.keys().zip(globals).map(|(name, global)| {
      (name.clone(), global as *const UnsafeCell<Global<'ivm>> as *const Global<'ivm>)
    }));

    let mut serializer = Serializer {
      host: self,
      current: Default::default(),
      equivalences: Default::default(),
      registers: Default::default(),
    };

    for (i, net) in nets.values().enumerate() {
      // Safety: essentially a `Cell::take`.
      serializer.current = take(unsafe { &mut *globals[i].get() });
      serializer.serialize_net(net);
      // Safety: essentially a `Cell::set`.
      unsafe { *globals[i].get() = take(&mut serializer.current) };
    }

    // Finally, we end the interior mutability, "freezing" the structure.
    unsafe { _ = &*globals_mut };

    PropagateLabels { mutable: globals.as_ptr_range() }.visit_all(globals);
  }
}

struct Serializer<'host, 'ast, 'ivm> {
  host: &'host mut Host<'ivm>,
  current: Global<'ivm>,
  equivalences: BTreeMap<&'ast str, &'ast str>,
  registers: HashMap<&'ast str, Register>,
}

impl<'l, 'ast, 'ivm> Serializer<'l, 'ast, 'ivm> {
  fn push(&mut self, instruction: Instruction<'ivm>) {
    unsafe { self.current.instructions.push(instruction) }
  }

  fn serialize_net(&mut self, net: &'ast Net) {
    self.equivalences.clear();
    self.registers.clear();

    for (a, b) in &net.pairs {
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

    if let Tree::Var(a) = &net.root {
      self.registers.insert(a, Register::ROOT);
      if let Some(b) = self.equivalences.get(&**a) {
        self.registers.insert(b, Register::ROOT);
      }
    }
    for (a, b) in net.pairs.iter().rev() {
      self.serialize_pair(a, b);
    }
    if !matches!(net.root, Tree::Var(_)) {
      self.serialize_tree_to(&net.root, Register::ROOT);
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
      Tree::N32(num) => {
        self.push(Instruction::Nilary(to, Port::new_ext_val(self.host.new_n32(*num))))
      }
      Tree::F32(num) => {
        self.push(Instruction::Nilary(to, Port::new_ext_val(self.host.new_f32(*num))))
      }
      Tree::Comb(label, a, b) => {
        let label = self.host.label_to_u16(label);
        let a = self.serialize_tree(a);
        let b = self.serialize_tree(b);
        self.push(Instruction::Binary(Tag::Comb, label, to, a, b));
      }
      Tree::ExtFn(f, swap, a, b) => {
        let a = self.serialize_tree(a);
        let b = self.serialize_tree(b);
        let ext_fn = self.host.instantiate_ext_fn(f, *swap);
        self.push(Instruction::Binary(Tag::ExtFn, ext_fn.bits(), to, a, b));
      }
      Tree::Global(name) => {
        let global = self.host.get_raw(name).expect("undefined global");
        // Safety: upholds the requirements of `Tag::Global`, and preserves the interior
        // mutability.
        let port = unsafe {
          Port::new(Tag::Global, 0, Addr(global as *const UnsafeCell<Global> as *const ()))
        };
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
    }
  }
}

struct PropagateLabels<'ivm> {
  mutable: Range<*const UnsafeCell<Global<'ivm>>>,
}

impl<'ivm> Bicycle for PropagateLabels<'ivm> {
  type Node = &'ivm UnsafeCell<Global<'ivm>>;

  fn state(&mut self, cur: Self::Node) -> &BicycleState {
    unsafe { &*(&mut (*cur.get()).flag as *mut usize as *const BicycleState) }
  }

  fn visit(&mut self, cur: Self::Node, mut recurse: impl FnMut(&mut Self, Self::Node)) {
    // Safety: once we get to `PropagateLabels`, we are no longer mutating thw
    // `instructions`, only the `labels`, so it is safe to create an
    // `&Instructions` that's held for the entirety of this function.
    let instructions = unsafe { (*cur.get()).instructions.instructions() };

    for i in instructions {
      match i {
        Instruction::Nilary(_, p) if p.tag() == Tag::Global => {
          let child = p.addr().0.cast::<Global>();
          if !ptr::addr_eq(child, cur) {
            if self.mutable.contains(&child.cast::<_>()) {
              // Safety: `child` is one of the globals we're currently
              // serializing, so it is interior mutable.
              recurse(self, unsafe { &*child.cast::<UnsafeCell<Global>>() });
            }
            // Safety: we know `child` and `cur` are different pointers, and
            // nothing else is accessing `labels`, so we can safely mutate
            // these.
            unsafe { (*cur.get()).labels.union(&(*child).labels) }
          }
        }
        Instruction::Binary(Tag::Comb, label, ..) => unsafe { (*cur.get()).labels.add(*label) },
        _ => {}
      }
    }
  }
}
