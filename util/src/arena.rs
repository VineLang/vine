#![allow(clippy::mut_from_ref)]

use std::{
  alloc::{alloc, Layout},
  cell::{Cell, UnsafeCell},
  ptr::{self, NonNull},
  slice, str,
};

pub struct BytesArena {
  cur: Cell<*mut [u8]>,
  chunks: UnsafeCell<Vec<*mut [u8]>>,
}

const PAGE: usize = 1024;
const HUGE_PAGE: usize = 2 * 1024 * 1024;

impl Default for BytesArena {
  fn default() -> Self {
    Self {
      cur: Cell::new(ptr::slice_from_raw_parts_mut(NonNull::dangling().as_ptr(), 0)),
      chunks: Default::default(),
    }
  }
}

impl BytesArena {
  pub fn alloc(&self, bytes: &[u8]) -> &mut [u8] {
    unsafe {
      let ptr = self.alloc_raw(bytes.len());
      ptr::copy_nonoverlapping(bytes as *const [u8] as *const u8, ptr, bytes.len());
      slice::from_raw_parts_mut(ptr, bytes.len())
    }
  }

  pub fn alloc_str(&self, str: &str) -> &str {
    unsafe { str::from_utf8_unchecked(self.alloc(str.as_bytes())) }
  }

  fn alloc_raw(&self, len: usize) -> *mut u8 {
    unsafe {
      self.reserve(len);
      let ptr = self.cur.get() as *mut u8;
      self.cur.set(ptr::slice_from_raw_parts_mut(
        (self.cur.get() as *mut u8).add(len),
        self.cur.get().len() - len,
      ));
      ptr
    }
  }

  pub fn reserve(&self, len: usize) {
    if len >= self.cur.get().len() {
      self.grow(len)
    }
  }

  fn grow(&self, len: usize) {
    unsafe {
      let chunks = &mut *self.chunks.get();
      let size = chunks.last().map(|x| (x.len() * 2).min(HUGE_PAGE)).unwrap_or(PAGE).max(len);
      let chunk = alloc(Layout::array::<u8>(size).unwrap());
      let chunk = ptr::slice_from_raw_parts_mut(chunk, size);
      self.cur.set(chunk);
      chunks.push(chunk);
    }
  }
}

impl Drop for BytesArena {
  fn drop(&mut self) {
    for ptr in self.chunks.get_mut().iter_mut() {
      unsafe { ptr::drop_in_place(ptr as *mut *mut [u8] as *mut Box<[u8]>) };
    }
  }
}

#[test]
fn test() {
  let arena = BytesArena::default();
  let data = [&[][..], &[1, 2, 3], include_bytes!("./arena.rs"), &[46; 4646]];
  let allocs = data.map(|x| arena.alloc(x));
  for (data, alloc) in data.into_iter().zip(allocs) {
    assert_eq!(data, alloc);
  }
}
