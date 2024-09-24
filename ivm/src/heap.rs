use core::{alloc::Layout, mem::transmute, ptr, slice::SliceIndex};
use std::alloc::alloc;

use crate::word::AtomicWord;

/// The heap, the primary memory of an IVM.
#[repr(transparent)]
pub struct Heap(pub(crate) [Node]);

/// An aligned region of memory storing two atomic words.
#[repr(align(16), C)]
pub(crate) struct Node(pub AtomicWord, pub AtomicWord);

/// The maximum size of heap to attempt to allocate.
pub const HEAP_SIZE_BYTES: usize = if cfg!(miri) {
  1 << 16
} else if cfg!(target_pointer_width = "64") {
  1 << 40
} else {
  1 << 30
};

impl Heap {
  /// Allocates a new heap, as large as possible.
  pub fn new() -> Box<Self> {
    let mut bytes = HEAP_SIZE_BYTES;
    while bytes != 0 {
      if let Some(heap) = Self::with_size(bytes) {
        return heap;
      }
      bytes /= 2;
    }
    panic!("heap allocation failed")
  }

  /// Attempts to allocate a new heap with a given size in bytes.
  ///
  /// Returns `None` if the allocation fails.
  pub fn with_size(bytes: usize) -> Option<Box<Self>> {
    let nodes = bytes / 16;
    if nodes == 0 {
      return None;
    }
    unsafe {
      let ptr = alloc(Layout::array::<Node>(nodes).unwrap()) as *mut Node;
      if ptr.is_null() {
        return None;
      }
      Some(Box::from_raw(ptr::slice_from_raw_parts_mut(ptr, nodes) as *mut Heap))
    }
  }

  pub(crate) fn slice(&self, range: impl SliceIndex<[Node], Output = [Node]>) -> &Self {
    unsafe { transmute::<&[Node], &Heap>(&self.0[range]) }
  }
}
