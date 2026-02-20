use crate::runtime::{
  Runtime,
  addr::Addr,
  heap::{Heap, Node},
  port::{Port, Tag},
  wire::Wire,
  word::Word,
};

pub(crate) struct Allocator<'ivm> {
  /// The unallocated memory in the heap backing this allocator.
  pub(crate) empty: &'ivm mut Heap,
  /// The head pointer of this allocator's freelist.
  pub(crate) head: Addr,
}

impl<'ivm> Allocator<'ivm> {
  pub(crate) fn new(empty: &'ivm mut Heap) -> Self {
    Allocator { empty, head: Addr::NULL }
  }

  pub(crate) fn fork(&mut self, index: usize, count: usize) -> Self {
    Allocator::new(self.empty.chunk(self.empty.0.len() / (count - index + 1)))
  }
}

impl<'ivm> Runtime<'ivm, '_> {
  /// Allocates a new binary node with a given `tag` and `label`.
  ///
  /// ## Safety
  /// `tag` must be the tag of a binary node, and `label` must comply with its
  /// requirements.
  #[inline(always)]
  pub unsafe fn new_node(&mut self, tag: Tag, label: u16) -> (Port<'ivm>, Wire<'ivm>, Wire<'ivm>) {
    unsafe {
      let addr = self.alloc_node();
      (Port::new(tag, label, addr), Wire::from_addr(addr), Wire::from_addr(addr.other_half()))
    }
  }

  /// Allocates a new wire, returning both of its ends.
  pub fn new_wire(&mut self) -> (Wire<'ivm>, Wire<'ivm>) {
    unsafe {
      let addr = self.alloc_node();
      self.free_wire(Wire::from_addr(addr.other_half()));
      (Wire::from_addr(addr), Wire::from_addr(addr))
    }
  }

  /// Allocates two new wires, returning both of their ends.
  pub fn new_wires(&mut self) -> ((Wire<'ivm>, Wire<'ivm>), (Wire<'ivm>, Wire<'ivm>)) {
    unsafe {
      let addr = self.alloc_node();
      (
        (Wire::from_addr(addr), Wire::from_addr(addr)),
        (Wire::from_addr(addr.other_half()), Wire::from_addr(addr.other_half())),
      )
    }
  }

  /// Frees the memory backing a wire.
  #[inline]
  pub(crate) fn free_wire(&mut self, wire: Wire<'ivm>) {
    self.stats.mem_free += 1;
    unsafe {
      let free = Word::from_bits(!0b111);
      let addr = wire.addr();
      addr.as_word().store(free);
      if addr.other_half().as_word().load() == free {
        let addr = addr.left_half();
        if addr.as_word().compare_exchange(free, Word::from_ptr(self.alloc.head.0)).is_ok() {
          self.alloc.head = addr;
        }
      }
    }
  }

  /// Allocates a two-word region of memory within the heap.
  #[inline]
  fn alloc_node(&mut self) -> Addr {
    self.stats.mem_alloc += 2;
    let addr = if self.alloc.head != Addr::NULL {
      let addr = self.alloc.head;
      let next = unsafe { Addr(addr.as_word().load().ptr()) };
      self.alloc.head = next;
      addr
    } else {
      self.stats.mem_heap += 2;
      let Some(node) = self.alloc.empty.bump() else {
        self.alloc = self.alloc_pool.pop().expect("OOM");
        return self.alloc_node();
      };
      Addr(node as *const Node as *const ())
    };
    unsafe { addr.as_word().store(Word::from_bits(0)) }
    unsafe { addr.other_half().as_word().store(Word::from_bits(0)) }
    addr
  }

  /// Allocates a pair in the heap, returning its address.
  pub fn make_pair(&mut self, a: Word, b: Word) -> Addr {
    unsafe {
      let addr = self.alloc_node();
      addr.as_word().store(a);
      addr.other_half().as_word().store(b);
      addr
    }
  }

  /// ## Safety
  /// `addr` must be the address of an allocated pair.
  pub unsafe fn take_pair(&mut self, addr: Addr) -> (Word, Word) {
    unsafe {
      let a = addr.as_word().swap(Word::from_ptr(self.alloc.head.0));
      let b = addr.other_half().as_word().load();
      self.alloc.head = addr;
      self.stats.mem_free += 2;
      (a, b)
    }
  }
}
