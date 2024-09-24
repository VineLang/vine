use core::{
  fmt::{self, Debug},
  ptr,
};

use crate::{
  global::Global,
  word::{AtomicWord, Word},
};

/// The address of a port, a pointer of only-externally-known interpretation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Addr(pub *const ());

unsafe impl Send for Addr {}
unsafe impl Sync for Addr {}

impl Addr {
  pub const NULL: Addr = Addr(ptr::null());

  /// Casts this address to an `&'ivm AtomicWord`.
  ///
  /// ## Safety
  /// This address must be a pointer into the heap of the `IVM<'ivm>`.
  #[inline(always)]
  pub(crate) unsafe fn as_word<'ivm>(self) -> &'ivm AtomicWord {
    unsafe { &*self.0.cast() }
  }

  /// Casts this address to an `&'ivm Global<'ivm>`.
  ///
  /// ## Safety
  /// This address must be the address of a `Port<'ivm>` with `Tag::Global`.
  #[inline(always)]
  pub unsafe fn as_global<'ivm>(self) -> &'ivm Global<'ivm> {
    unsafe { &*self.0.cast() }
  }

  const HALF_BIT: u64 = 0b1000;

  #[inline(always)]
  fn map_addr(self, f: impl FnOnce(u64) -> u64) -> Self {
    Addr(Word::from_ptr(self.0).map_bits(f).ptr())
  }

  /// Returns the address of the other word in a two-word allocation.
  #[inline(always)]
  pub fn other_half(self) -> Self {
    self.map_addr(|x| x ^ Self::HALF_BIT)
  }

  /// Returns the address of the left word in a two-word allocation.
  #[inline(always)]
  pub fn left_half(self) -> Self {
    self.map_addr(|x| x & !Self::HALF_BIT)
  }
}

impl Debug for Addr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:012?}", self.0)
  }
}
