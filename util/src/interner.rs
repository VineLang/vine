use std::{
  cell::UnsafeCell,
  cmp::Ordering,
  fmt::{self, Debug},
  hash::{Hash, Hasher},
  ops::Deref,
  ptr,
};

use hashbrown::HashMap;

use crate::arena::BytesArena;

pub struct StringInterner<'a> {
  arena: &'a BytesArena,
  strs: UnsafeCell<HashMap<&'static str, ()>>,
}

impl<'a> StringInterner<'a> {
  pub fn new(arena: &'a BytesArena) -> Self {
    Self { arena, strs: Default::default() }
  }

  pub fn intern(&self, str: &str) -> Interned<'a, str> {
    let strs = unsafe { &mut *self.strs.get().cast::<HashMap<&'a str, ()>>() };
    Interned(
      strs.raw_entry_mut().from_key(str).or_insert_with(|| (self.arena.alloc_str(str), ())).0,
      (),
    )
  }
}

#[allow(clippy::manual_non_exhaustive)]
pub struct Interned<'a, T: ?Sized>(pub &'a T, ());

impl<'a, T: ?Sized + Debug> Debug for Interned<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    Debug::fmt(self.0, f)
  }
}

impl<'a, T: ?Sized> Clone for Interned<'a, T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<'a, T: ?Sized> Copy for Interned<'a, T> {}

impl<'a, T: ?Sized> PartialEq for Interned<'a, T> {
  fn eq(&self, other: &Self) -> bool {
    ptr::eq(self.0, other.0)
  }
}

impl<'a, T: ?Sized> Eq for Interned<'a, T> {}

impl<'a, T: ?Sized + PartialOrd> PartialOrd for Interned<'a, T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    if ptr::eq(self.0, other.0) {
      Some(Ordering::Equal)
    } else {
      self.0.partial_cmp(other.0)
    }
  }
}

impl<'a, T: ?Sized + Ord> Ord for Interned<'a, T> {
  fn cmp(&self, other: &Self) -> Ordering {
    if ptr::eq(self.0, other.0) {
      Ordering::Equal
    } else {
      self.0.cmp(other.0)
    }
  }
}

impl<'a, T: ?Sized> Hash for Interned<'a, T> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    ptr::hash(self.0, state)
  }
}

impl<'a, T: ?Sized> Deref for Interned<'a, T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    self.0
  }
}

#[test]
fn test() {
  let arena = BytesArena::default();
  let interner = StringInterner::new(&arena);
  let strs =
    ["", "\0", "abcdefg", include_str!("./interner.rs"), "abcdefg", "123", "123", "", "\0"];
  let interned = strs.map(|x| interner.intern(x));
  for (str, interned) in strs.into_iter().zip(interned) {
    assert_eq!(str, &*interned);
  }
  for (a_str, a_interned) in strs.into_iter().zip(interned) {
    for (b_str, b_interned) in strs.into_iter().zip(interned) {
      assert_eq!((a_interned == b_interned), (a_str == b_str));
    }
  }
}
