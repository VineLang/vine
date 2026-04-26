//! Conversion of Ivy nets to IVM [`Graft`]s.
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
//! create `&UnsafeCell<Graft>` references; we need to ensure there that they
//! are never derived from an `&Graft` reference, as that would be UB. Thus,
//! many convenience methods from IVM for dealing with grafts are unusable, as
//! they consistently use `&Graft`s. Instead, we must use the lower-level
//! raw-pointer APIs. In particular, up until the point of freezing, we treat
//! the address of a [`Tag::Graft`] port as an `&UnsafeCell<Graft>` if it's one
//! of the grafts we're currently creating, rather than the usual `&Graft`.

use std::{
  cell::UnsafeCell,
  collections::{BTreeSet, HashMap, hash_map::Entry},
};

use ivy::{
  name::{FromTable, NameId, Table},
  net::{FlatNet, TreeNet, TreeNode, Wire},
};

use crate::{
  host::{
    Host,
    ext::{
      ExtTyRegister,
      common::{Branches, Nil},
    },
  },
  runtime::{
    addr::Addr,
    ext::ExtTyCastStatic,
    graft::{Graft, Instruction, Register},
    port::{Port, Tag},
  },
  system::Guide,
};

#[derive(Default)]
pub struct Program<'ivm> {
  grafts: HashMap<NameId, *const Graft<'ivm>>,
}

impl<'ivm> Program<'ivm> {
  pub fn new(host: &Host<'ivm>, table: &mut Table, nets: &HashMap<NameId, FlatNet>) -> Self {
    let mut program = Program::default();
    program.build(host, table, nets);
    program
  }

  pub fn graft(&self, name: NameId) -> Option<&'ivm Graft<'ivm>> {
    Some(unsafe { &**self.grafts.get(&name)? })
  }

  pub fn build(&mut self, host: &Host<'ivm>, table: &mut Table, nets: &HashMap<NameId, FlatNet>) {
    let guide = &Guide::build(table);

    let names = BTreeSet::from_iter(nets.keys().copied());

    let grafts =
      host.ivm.grafts.push((0..nets.len()).map(|_| Graft::new()).collect::<Vec<_>>().into());

    // Here, we begin the period of temporary interior mutability. Converting a
    // mutable unique reference to an interior-mutable shared reference is a
    // safe operation (cf. `UnsafeCell::from_mut`).
    let grafts = unsafe { &*(grafts as *mut [Graft<'ivm>] as *const [UnsafeCell<Graft<'ivm>>]) };

    for (&name, graft) in names.iter().zip(grafts) {
      self.grafts.insert(name, graft as *const UnsafeCell<Graft<'ivm>> as *const Graft<'ivm>);
    }

    let mut _graft = Graft::new();
    let mut encoder = Encoder {
      host,
      table,
      guide,
      grafts: &self.grafts,
      graft: &mut _graft,
      registers: Default::default(),
      active: Default::default(),
      free_registers: Default::default(),
    };

    for (&name, graft) in names.iter().zip(grafts) {
      let net = &nets[&name];
      let mut net = TreeNet::from_flat(net);
      for (_, node) in &mut net.links {
        if let TreeNode::Node(name, children) = node
          && name.path == guide.black_box
        {
          *node = children.pop().unwrap();
        }
      }
      net.resolve_links();
      encoder.graft = unsafe { &mut *graft.get() };
      encoder.encode(net);
    }
  }
}

struct Encoder<'ivm, 'a> {
  host: &'a Host<'ivm>,
  table: &'a Table,
  guide: &'a Guide,
  grafts: &'a HashMap<NameId, *const Graft<'ivm>>,
  graft: &'a mut Graft<'ivm>,
  registers: HashMap<Wire, Register>,
  active: HashMap<Register, ()>,
  free_registers: Vec<Register>,
}

impl<'ivm> Encoder<'ivm, '_> {
  fn encode(&mut self, net: TreeNet) {
    let [root] = net.free.try_into().unwrap();
    self.active.insert(Register::ROOT, ());
    match root {
      TreeNode::Wire(wire) => {
        self.registers.insert(wire, Register::ROOT);
      }
      tree => {
        self.encode_tree(tree, Register::ROOT);
      }
    }
    for (a, b) in net.links {
      assert!(!matches!(b, TreeNode::Wire(_)));
      let reg = self.pick_register(&a);
      self.encode_tree(a, reg);
      self.encode_tree(b, reg);
    }
    assert!(self.registers.is_empty());
    assert!(self.active.is_empty());
    self.free_registers.clear();
  }

  fn register(&mut self) -> Register {
    self.free_registers.pop().unwrap_or_else(|| self.graft.new_register())
  }

  fn push(&mut self, instruction: Instruction<'ivm>) {
    for &reg in instruction.registers() {
      match self.active.entry(reg) {
        Entry::Occupied(e) => {
          e.remove();
          self.free_registers.push(reg);
        }
        Entry::Vacant(e) => {
          e.insert(());
        }
      }
    }
    unsafe { self.graft.push(instruction) };
  }

  fn pick_register(&mut self, tree: &TreeNode) -> Register {
    match *tree {
      TreeNode::Wire(wire) => match self.registers.entry(wire) {
        Entry::Occupied(e) => e.remove(),
        Entry::Vacant(e) => {
          let reg = self.free_registers.pop().unwrap_or_else(|| self.graft.new_register());
          e.insert(reg);
          reg
        }
      },
      TreeNode::Node(..) => self.register(),
    }
  }

  fn encode_tree(&mut self, tree: TreeNode, reg: Register) {
    match tree {
      TreeNode::Wire(_) => {}
      TreeNode::Node(name, children) => {
        let path = name.path;
        match () {
          _ if path == self.guide.eraser => {
            assert!(children.is_empty());
            self.push(Instruction::Nilary(Port::ERASE, reg));
          }
          _ if path == self.guide.graft => {
            assert!(children.is_empty());
            let addr = Addr(self.grafts[&name.children[0]] as *const ());
            self.push(Instruction::Nilary(unsafe { Port::new(Tag::Graft, 0, addr) }, reg));
          }
          _ if path == self.guide.x || path == self.guide.y => {
            let [a, b] = children.try_into().unwrap();
            let label = (path == self.guide.y) as u16;
            self.encode_binary(Tag::Comb, label, reg, a, b);
          }
          _ if path == self.guide.ext_split
            || path == self.guide.ext_merge
            || path == self.guide.ext_merge_swap =>
          {
            let [a, b] = children.try_into().unwrap();
            let fn_name = name.children[0];
            let ext_fn = if path == self.guide.ext_split {
              self.host.ext_split_lookup.get(&fn_name)
            } else {
              self.host.ext_merge_lookup.get(&fn_name)
            };
            let Some(&ext_fn) = ext_fn else {
              panic!(
                "unknown extrinsic {} fn {:?}",
                if path == self.guide.ext_merge { "merge" } else { "split" },
                self.table.show_name_id(fn_name)
              );
            };
            let ext_fn = if path == self.guide.ext_merge_swap { ext_fn.swapped() } else { ext_fn };
            self.encode_binary(Tag::ExtFn, ext_fn.bits(), reg, a, b);
          }
          _ if path == self.guide.n32 => {
            self.encode_ext_val(reg, name.payload.as_u32().unwrap());
          }
          _ if path == self.guide.f32 => {
            self.encode_ext_val(reg, f32::from_bits(name.payload.as_u32().unwrap()));
          }
          _ if path == self.guide.nil => {
            self.encode_ext_val(reg, Nil);
          }
          _ if path == self.guide.branch => {
            assert!(children.is_empty());
            let branch = name
              .children
              .iter()
              .map(|g| self.grafts[g])
              .collect::<Vec<*const Graft<'ivm>>>()
              .into();
            self.encode_ext_val(reg, Branches(self.host.ivm.branches.push(Box::new(branch))));
          }
          _ => panic!("unknown node `{}`", self.table.show_name(&name)),
        }
      }
    }
  }

  fn encode_ext_val<T: ExtTyRegister<'ivm> + ExtTyCastStatic<'ivm>>(
    &mut self,
    reg: Register,
    value: T,
  ) {
    assert!(<T as ExtTyCastStatic>::COPY);
    let ty = self.host.get_ext_ty().unwrap();
    let value = ty.wrap_static(value);
    self.push(Instruction::Nilary(Port::new_ext_val(value), reg));
  }

  fn encode_binary(&mut self, tag: Tag, label: u16, reg: Register, a: TreeNode, b: TreeNode) {
    let ra = self.pick_register(&a);
    let rb = self.pick_register(&b);
    self.push(Instruction::Binary(tag, label, [reg, ra, rb]));
    self.encode_tree(a, ra);
    self.encode_tree(b, rb);
  }
}
