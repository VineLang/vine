use core::slice;
use std::{
  collections::{BTreeMap, btree_map},
  iter,
  mem::replace,
  vec,
};

use crate::idx::{Idx, RangeExt, RangeIter};

pub struct RangeMap<I: Idx, T> {
  map: BTreeMap<I, Vec<T>>,
}

impl<I: Idx, T> RangeMap<I, T> {
  pub fn get(&self, idx: I) -> Option<&T> {
    let (start, vec) = self.map.range(..=idx).next_back()?;
    vec.get(idx.index() - start.index())
  }

  pub fn get_mut(&mut self, idx: I) -> Option<&mut T> {
    let (start, vec) = self.map.range_mut(..=idx).next_back()?;
    vec.get_mut(idx.index() - start.index())
  }

  pub fn insert(&mut self, idx: I, value: T) -> Option<T> {
    let (start, vec) = self.map.range_mut(..=idx).next_back()?;
    let i = idx.index() - start.index();
    if let Some(slot) = vec.get_mut(i) {
      Some(replace(slot, value))
    } else if i == vec.len() {
      vec.push(value);
      None
    } else {
      self.map.insert(idx, vec![value]);
      None
    }
  }

  pub fn values(&self) -> iter::Flatten<btree_map::Values<'_, I, Vec<T>>> {
    self.map.values().flatten()
  }

  pub fn values_mut(&mut self) -> iter::Flatten<btree_map::ValuesMut<'_, I, Vec<T>>> {
    self.map.values_mut().flatten()
  }
}

impl<I: Idx, T> Default for RangeMap<I, T> {
  fn default() -> Self {
    Self { map: Default::default() }
  }
}

pub type IntoIter<I, T> = iter::FlatMap<
  btree_map::IntoIter<I, vec::Vec<T>>,
  iter::Zip<RangeIter<I>, vec::IntoIter<T>>,
  fn((I, Vec<T>)) -> iter::Zip<RangeIter<I>, vec::IntoIter<T>>,
>;

impl<I: Idx, T> IntoIterator for RangeMap<I, T> {
  type Item = (I, T);
  type IntoIter = IntoIter<I, T>;

  fn into_iter(self) -> Self::IntoIter {
    self
      .map
      .into_iter()
      .flat_map(|(start, vec)| (start..I::from(start.index() + vec.len())).iter().zip(vec))
  }
}

pub type Iter<'a, I, T> = iter::FlatMap<
  btree_map::Iter<'a, I, vec::Vec<T>>,
  iter::Zip<RangeIter<I>, slice::Iter<'a, T>>,
  fn((&'a I, &'a Vec<T>)) -> iter::Zip<RangeIter<I>, slice::Iter<'a, T>>,
>;

impl<'a, I: Idx, T> IntoIterator for &'a RangeMap<I, T> {
  type Item = (I, &'a T);
  type IntoIter = Iter<'a, I, T>;

  fn into_iter(self) -> Self::IntoIter {
    self
      .map
      .iter()
      .flat_map(|(start, vec)| (*start..I::from(start.index() + vec.len())).iter().zip(vec))
  }
}

pub type IterMut<'a, I, T> = iter::FlatMap<
  btree_map::IterMut<'a, I, vec::Vec<T>>,
  iter::Zip<RangeIter<I>, slice::IterMut<'a, T>>,
  fn((&'a I, &'a mut Vec<T>)) -> iter::Zip<RangeIter<I>, slice::IterMut<'a, T>>,
>;

impl<'a, I: Idx, T> IntoIterator for &'a mut RangeMap<I, T> {
  type Item = (I, &'a mut T);
  type IntoIter = IterMut<'a, I, T>;

  fn into_iter(self) -> Self::IntoIter {
    self
      .map
      .iter_mut()
      .flat_map(|(start, vec)| (*start..I::from(start.index() + vec.len())).iter().zip(vec))
  }
}
