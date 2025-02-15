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
  collections::{hash_map::Entry, HashMap},
  mem::take,
  ops::Range,
  ptr,
};

use ivm::{
  addr::Addr,
  ext::ExtVal,
  global::{Global, LabelSet},
  port::{Port, Tag},
  word::Word,
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
      nodes: Vec::new(),
      pairs: Vec::new(),
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

    let mut serializer =
      Serializer { host: self, nodes: Vec::new(), pairs: Vec::new(), vars: Default::default() };

    for (i, net) in nets.values().enumerate() {
      // Safety: essentially a `Cell::take`.
      serializer.serialize_net(net);
      // Safety: essentially a `Cell::set`.
      unsafe { (*globals[i].get()).nodes = take(&mut serializer.nodes) };
      unsafe { (*globals[i].get()).pairs = take(&mut serializer.pairs) };
    }

    // Finally, we end the interior mutability, "freezing" the structure.
    unsafe { _ = &*globals_mut };

    PropagateLabels { mutable: globals.as_ptr_range() }.visit_all(globals);
  }
}

struct Serializer<'host, 'ast, 'ivm> {
  host: &'host mut Host<'ivm>,
  nodes: Vec<[Option<Port<'ivm>>; 2]>,
  pairs: Vec<[Option<Port<'ivm>>; 2]>,
  vars: HashMap<&'ast str, Option<Port<'ivm>>>,
}

impl<'l, 'ast, 'ivm> Serializer<'l, 'ast, 'ivm> {
  fn serialize_net(&mut self, net: &'ast Net) {
    self.vars.clear();

    if let Tree::Var(v) = &net.root {
      self.vars.insert(&**v, None);
    } else {
      let r = self.bar(&net.root);
      self.pairs.push([None, r]);
    };

    for (a, b) in net.pairs.iter() {
      self.serialize_pair(a, b);
    }
    self.pairs.reverse();
  }

  fn serialize_pair(&mut self, a: &'ast Tree, b: &'ast Tree) {
    let a = self.bar(a);
    let b = self.bar(b);
    self.pairs.push([a, b]);
  }

  fn foo(&mut self, tree: &'ast Tree, idx: usize, p: usize) {
    let port = unsafe {
      Port::from_bits(Word::from_bits((Tag::Wire as u64) | (idx << 4) as u64 | (p << 3) as u64))
    };
    if let Tree::Var(v) = tree {
      match self.vars.entry(v) {
        Entry::Occupied(e) => {
          if let Some(x) = e.remove() {
            self.nodes[idx][p] = Some(x);
          } else {
            self.pairs.push([None, Some(port)]);
          }
        }
        Entry::Vacant(e) => {
          e.insert(Some(port));
        }
      }
    } else {
      self.nodes[idx][p] = Some(self.bar(tree).unwrap());
    }
  }

  fn new_node(&mut self) -> usize {
    let i = self.nodes.len();
    self.nodes.push([None, None]);
    i
  }

  fn pri(tag: Tag, label: u16, idx: usize) -> Port<'ivm> {
    unsafe {
      Port::from_bits(Word::from_bits((tag as u64) | ((label as u64) << 48) | ((idx as u64) << 4)))
    }
  }

  fn bar(&mut self, tree: &'ast Tree) -> Option<Port<'ivm>> {
    Some(match tree {
      Tree::Erase => Port::ERASE,
      Tree::N32(num) => Port::new_ext_val(ExtVal::new_n32(*num)),
      Tree::F32(num) => Port::new_ext_val(ExtVal::new_f32(*num)),
      Tree::Comb(label, a, b) => {
        let node = self.new_node();
        self.foo(a, node, 0);
        self.foo(b, node, 1);
        let label = self.host.label_to_u16(label);
        Self::pri(Tag::Comb, label, node)
      }
      Tree::ExtFn(f, a, b) => {
        let node = self.new_node();
        self.foo(a, node, 0);
        self.foo(b, node, 1);
        Self::pri(Tag::ExtFn, f.bits(), node)
      }
      Tree::Global(name) => {
        let global = self.host.get_raw(name).expect("undefined global");
        // Safety: upholds the requirements of `Tag::Global`, and preserves the interior
        // mutability.
        unsafe { Port::new(Tag::Global, 0, Addr(global as *const UnsafeCell<Global> as *const ())) }
      }
      Tree::Branch(z, p, o) => {
        let x = self.new_node();
        self.foo(z, x, 0);
        self.foo(p, x, 1);
        let y = self.new_node();
        self.nodes[y][0] = Some(Self::pri(Tag::Branch, 0, x));
        self.foo(o, y, 1);
        Self::pri(Tag::Branch, 0, y)
      }
      Tree::Var(v) => self.vars.remove(&**v).unwrap()?,
    })
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
    let nodes = unsafe { &(*cur.get()).nodes };
    let pairs = unsafe { &(*cur.get()).pairs };

    let ports = [nodes, pairs].into_iter().flatten().flatten().flatten();

    for p in ports {
      match p.tag() {
        Tag::Global => {
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
        Tag::Comb => unsafe { (*cur.get()).labels.add(p.label()) },
        _ => {}
      }
    }
  }
}
