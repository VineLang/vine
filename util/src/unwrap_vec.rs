use core::ptr;

use crate::idx::{Idx, IdxVec};

pub fn unwrap_vec<T>(vec: Vec<Option<T>>) -> Vec<T> {
  const { assert!(size_of::<T>() == size_of::<Option<T>>()) };
  for el in &vec {
    assert!(el.is_some());
  }
  let slice = Box::into_raw(vec.into_boxed_slice());
  let slice = ptr::slice_from_raw_parts_mut(slice.cast(), slice.len());
  unsafe { Vec::from(Box::from_raw(slice)) }
}

pub fn unwrap_idx_vec<I: Idx, T>(vec: IdxVec<I, Option<T>>) -> IdxVec<I, T> {
  unwrap_vec(vec.into()).into()
}
