//! Zero-cost wrappers around `u64` and derivatives that properly preserve
//! [provenance] information for miri.
//!
//! [provenance]: https://doc.rust-lang.org/nightly/std/ptr/index.html#provenance

// When running under miri, we need to preserve provenance information, which we
// do by storing words as pointers. If pointers are not 64-bit, this is not
// possible.
#[cfg(all(miri, not(target_pointer_width = "64")))]
compile_error!("miri is only supported with 64-bit pointers");

#[allow(unused_imports)]
use core::{
  num::NonZeroU64,
  ptr::{self, NonNull},
  sync::atomic::{AtomicPtr, AtomicU64, Ordering::Relaxed},
};

/// A 64-bit value with provenance.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub(crate) struct Word(#[cfg(not(miri))] u64, #[cfg(miri)] *mut ());

impl Word {
  /// Creates a new `Word` with the given `bits` and no provenance.
  #[cfg(not(miri))]
  #[inline(always)]
  pub const fn from_bits(bits: u64) -> Self {
    Word(bits)
  }

  /// Creates a new `Word` with the given `bits` and no provenance.
  #[cfg(miri)]
  #[inline(always)]
  pub const fn from_bits(bits: u64) -> Self {
    Word(ptr::without_provenance_mut(bits as usize))
  }

  /// Creates a new `Word` from a pointer, inheriting its bits and provenance.
  #[inline(always)]
  pub fn from_ptr(ptr: *const ()) -> Self {
    Word(ptr as _)
  }

  /// Returns the bits of this word.
  #[cfg(not(miri))]
  #[inline(always)]
  pub fn bits(self) -> u64 {
    self.0
  }

  /// Returns the bits of this word.
  #[cfg(miri)]
  #[inline(always)]
  pub fn bits(self) -> u64 {
    self.0.addr() as u64
  }

  /// Returns this word as a pointer, preserving the low bits and provenance.
  #[inline(always)]
  pub fn ptr(self) -> *const () {
    self.0 as *const ()
  }

  /// Transform the bits of this word whilst preserving its provenance.
  #[cfg(not(miri))]
  #[inline(always)]
  pub fn map_bits(self, f: impl FnOnce(u64) -> u64) -> Self {
    Word(f(self.0))
  }

  /// Transform the bits of this word whilst preserving its provenance.
  #[cfg(miri)]
  #[inline(always)]
  pub fn map_bits(self, f: impl FnOnce(u64) -> u64) -> Self {
    Word(self.0.map_addr(|x| f(x as u64) as usize))
  }
}

/// An atomic 64-bit value with provenance, with only [`Relaxed`] operations.
#[repr(transparent)]
pub(crate) struct AtomicWord(#[cfg(not(miri))] AtomicU64, #[cfg(miri)] AtomicPtr<()>);

impl AtomicWord {
  #[inline(always)]
  pub fn load(&self) -> Word {
    Word(self.0.load(Relaxed))
  }

  #[inline(always)]
  pub fn store(&self, word: Word) {
    self.0.store(word.0, Relaxed)
  }

  #[inline(always)]
  pub fn swap(&self, word: Word) -> Word {
    Word(self.0.swap(word.0, Relaxed))
  }

  #[inline(always)]
  pub fn compare_exchange(&self, current: Word, new: Word) -> Result<Word, Word> {
    self.0.compare_exchange(current.0, new.0, Relaxed, Relaxed).map(Word).map_err(Word)
  }
}

/// A non-zero 64-bit value with provenance.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub(crate) struct NonZeroWord(#[cfg(not(miri))] NonZeroU64, #[cfg(miri)] NonNull<()>);

impl NonZeroWord {
  #[cfg(not(miri))]
  #[inline(always)]
  pub const unsafe fn new_unchecked(word: Word) -> Self {
    NonZeroWord(NonZeroU64::new_unchecked(word.0))
  }

  #[cfg(miri)]
  #[inline(always)]
  pub const unsafe fn new_unchecked(word: Word) -> Self {
    NonZeroWord(NonNull::new_unchecked(word.0))
  }

  #[cfg(not(miri))]
  #[inline(always)]
  pub fn get(&self) -> Word {
    Word(self.0.get())
  }

  #[cfg(miri)]
  #[inline(always)]
  pub fn get(&self) -> Word {
    Word(self.0.as_ptr())
  }
}

unsafe impl Send for Word {}
unsafe impl Sync for Word {}
unsafe impl Send for NonZeroWord {}
unsafe impl Sync for NonZeroWord {}
