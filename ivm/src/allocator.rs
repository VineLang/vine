use crate::{
  addr::Addr,
  heap::Node,
  ivm::IVM,
  port::{Port, Tag},
  wire::Wire,
  word::Word,
};

impl<'ivm> IVM<'ivm> {
  /// Allocates a new binary node with a given `tag` and `label`.
  ///
  /// ## Safety
  /// `tag` must be the tag of a binary node, and `label` must comply with its
  /// requirements.
  #[inline(always)]
  pub(crate) unsafe fn new_node(
    &mut self,
    tag: Tag,
    label: u16,
  ) -> (Port<'ivm>, Wire<'ivm>, Wire<'ivm>) {
    let addr = self.alloc_node();
    (Port::new(tag, label, addr), Wire::from_addr(addr), Wire::from_addr(addr.other_half()))
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
        if addr.as_word().compare_exchange(free, Word::from_ptr(self.alloc_head.0)).is_ok() {
          self.alloc_head = addr;
        }
      }
    }
  }

  /// Allocates a two-word region of memory within the heap.
  #[inline]
  pub(crate) fn alloc_node(&mut self) -> Addr {
    self.stats.mem_alloc += 2;
    let addr = if self.alloc_head != Addr::NULL {
      let addr = self.alloc_head;
      let next = unsafe { Addr(addr.as_word().load().ptr()) };
      self.alloc_head = next;
      addr
    } else {
      self.stats.mem_heap += 2;
      let index = self.alloc_next;
      self.alloc_next += 1;
      let node = self.heap.0.get(index).expect("OOM");
      Addr(node as *const Node as *const ())
    };
    unsafe { addr.as_word().store(Word::from_bits(0)) }
    unsafe { addr.other_half().as_word().store(Word::from_bits(0)) }
    addr
  }
}
