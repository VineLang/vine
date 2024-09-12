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
//! the address of a [`Tag::Global`] port as an `&UnsafeCell<Global>`, *not* an
//! `&Global`.

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

impl Nets {
  /// Serializes these nets into IVM [`Global`]s, populating the supplied
  /// `global` buffer (which must be empty), and returning an immutable
  /// reference to the serialized globals.
  ///
  /// The indices of the returned global slice correspond exactly to the indices
  /// of the nets in the `IndexMap`.
  pub fn serialize<'ivm>(&self, globals: &'ivm mut Vec<Global<'ivm>>) -> &'ivm [Global<'ivm>] {
    assert!(globals.is_empty());
    globals.extend(self.keys().map(|name| Global {
      name: name.clone(),
      labels: LabelSet::NONE,
      instructions: Instructions::default(),
      flag: 0,
    }));
    let globals = &mut globals[..];

    // Here, we begin the period of temporary interior mutability. Converting a
    // mutable unique reference to an interior-mutable shared reference is a
    // safe operation (cf. `UnsafeCell::from_mut`).
    let globals =
      unsafe { transmute::<&'ivm mut [Global<'ivm>], &'ivm [UnsafeCell<Global<'ivm>>]>(globals) };

    let mut serializer = Serializer {
      globals,
      nets: self,
      current: Default::default(),
      equivalences: Default::default(),
      registers: Default::default(),
      labels: Default::default(),
    };

    for (i, net) in self.values().enumerate() {
      // Safety: essentially a `Cell::take`.
      serializer.current = take(unsafe { &mut *globals[i].get() });
      serializer.serialize_net(net);
      // Safety: essentially a `Cell::set`.
      unsafe { *globals[i].get() = take(&mut serializer.current) };
    }

    PropagateLabels(PhantomData).visit_all(globals);

    // Finally, we end the interior mutability, "freezing" the structure.
    unsafe { transmute::<&'ivm [UnsafeCell<Global<'ivm>>], &'ivm [Global<'ivm>]>(globals) }
  }
}

struct Serializer<'ast, 'ivm> {
  globals: &'ivm [UnsafeCell<Global<'ivm>>],
  nets: &'ast Nets,
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
        // Safety: upholds both the requirements of `Tag::Global`, and our
        // file-local invariant that `Tag::Global` ports are interior-mutable.
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
    // Safety: once we get to `PropagateLabels`, we are no longer mutating thw
    // `instructions`, only the `labels`, so it is safe to create an
    // `&Instructions` that's held for the entirety of this function.
    let instructions = unsafe { (*cur.get()).instructions.instructions() };

    for i in instructions {
      match i {
        Instruction::Nilary(_, p) if p.tag() == Tag::Global => {
          // Safety: guaranteed by the tag check and our file-local invariant
          // that `Tag::Global` ports are interior-mutable.
          let child = unsafe { &*p.addr().0.cast::<UnsafeCell<Global>>() };
          if !ptr::addr_eq(child, cur) {
            recurse(self, child);
            // Safety: we know `child` and `cur` are different pointers, and
            // nothing else is accessing `labels`, so we can safely mutate
            // these.
            unsafe { (*cur.get()).labels.union(&(*child.get()).labels) }
          }
        }
        _ => {}
      }
    }
  }
}
