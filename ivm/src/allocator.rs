use crate::{
  addr::Addr,
  heap::{Heap, Node},
  ivm::IVM,
  port::{Port, Tag},
  wire::Wire,
  word::Word,
};

pub(crate) struct Allocator<'ivm> {
  /// The heap memory backing this allocator.
  pub(crate) heap: &'ivm Heap,
  /// The head pointer of this allocator's freelist.
  pub(crate) head: Addr,
  /// The next unallocated index in the `heap`.
  pub(crate) next: usize,
}

impl<'ivm> Allocator<'ivm> {
  pub(crate) fn new(heap: &'ivm Heap) -> Self {
    Allocator { heap, head: Addr::NULL, next: 0 }
  }

  pub(crate) fn shrink(&mut self) {
    self.heap = self.heap.slice(self.next..);
    self.next = 0;
  }

  pub(crate) fn fork(&mut self, index: usize, count: usize) -> Self {
    self.shrink();
    let length = self.heap.0.len();
    let fork_point = length - length / (count + index + 1);
    let fork = self.heap.slice(fork_point..);
    self.heap = self.heap.slice(..fork_point);
    Self::new(fork)
  }
}

impl<'ivm, 'ext> IVM<'ivm, 'ext> {
  /// Allocates a new binary node with a given `tag` and `label`.
  ///
  /// ## Safety
  /// `tag` must be the tag of a binary node, and `label` must comply with its
  /// requirements.
  #[inline(always)]
  pub unsafe fn new_node(&mut self, tag: Tag, label: u16) -> (Port<'ivm>, Wire<'ivm>, Wire<'ivm>) {
    let addr = self.alloc_node();
    (Port::new(tag, label, addr), Wire::from_addr(addr), Wire::from_addr(addr.other_half()))
  }

  /// Allocates a new wire, returning both of its ends.
  pub fn new_wire(&mut self) -> (Wire<'ivm>, Wire<'ivm>) {
    unsafe {
      let addr = self.alloc_node();
      self.free_wire(Wire::from_addr(addr.other_half()));
      (Wire::from_addr(addr), Wire::from_addr(addr))
    }
  }

  /// Frees the memory backing a wire.
  #[inline]
  pub(crate) fn free_wire(&mut self, wire: Wire<'ivm>) {
    self.stats.mem_free += 1;
    unsafe {
      let free = Word::from_bits(u64::MAX & !0b111);
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
      let index = self.alloc.next;
      self.alloc.next += 1;
      if let Some(node) = self.alloc.heap.0.get(index) {
        Addr(node as *const Node as *const ())
      } else {
        self.alloc = self.alloc_pool.pop().expect("OOM");
        return self.alloc_node();
      }
    };
    unsafe { addr.as_word().store(Word::from_bits(0)) }
    unsafe { addr.other_half().as_word().store(Word::from_bits(0)) }
    addr
  }
}
