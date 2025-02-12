use core::fmt;
use std::{
  fmt::Debug,
  hash::Hash,
  iter::{Enumerate, Map},
  marker::PhantomData,
  ops::{Index, IndexMut, Range},
  slice, vec,
};

#[doc(hidden)]
pub use nohash_hasher::IsEnabled;

pub use nohash_hasher::{IntMap, IntSet};

pub trait Idx:
  Copy + Eq + Ord + Hash + Default + IsEnabled + From<usize> + Into<usize> + Debug
{
}
impl Idx for usize {}

#[macro_export]
macro_rules! new_idx {
  ($vis:vis $Ty:ident) => {
    $crate::new_idx!($vis $Ty; n => ["{n}"]);
  };
  ($vis:vis $Ty:ident; $n:ident => [$($fmt:tt)*]) => {
    #[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    $vis struct $Ty(pub usize);

    impl Into<usize> for $Ty {
      fn into(self) -> usize { self.0 }
    }

    impl From<usize> for $Ty {
      fn from(i: usize) -> Self { Self(i) }
    }

    impl $crate::idx::IsEnabled for $Ty {}
    impl $crate::idx::Idx for $Ty {}

    impl std::fmt::Debug for $Ty {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let $n = self.0;
        write!(f, $($fmt)*)
      }
    }
  };
}

#[allow(non_snake_case)]
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdxVec<I: Idx, T> {
  pub vec: Vec<T>,
  PhantomData: PhantomData<fn(&I)>,
}

impl<I: Idx, T> IdxVec<I, T> {
  #[inline(always)]
  pub const fn new() -> Self {
    IdxVec { vec: Vec::new(), PhantomData }
  }

  #[inline(always)]
  pub fn len(&self) -> usize {
    self.vec.len()
  }

  #[inline(always)]
  pub fn is_empty(&self) -> bool {
    self.vec.is_empty()
  }

  #[inline(always)]
  pub fn range(&self) -> Range<I> {
    0.into()..self.next_index()
  }

  #[inline(always)]
  pub fn next_index(&self) -> I {
    self.len().into()
  }

  #[inline(always)]
  pub fn push(&mut self, value: T) -> I {
    let index = self.next_index();
    self.vec.push(value);
    index
  }

  #[inline(always)]
  pub fn get(&self, index: I) -> Option<&T> {
    self.vec.get(index.into())
  }

  #[inline(always)]
  pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
    self.vec.get_mut(index.into())
  }

  #[inline(always)]
  pub fn last(&self) -> Option<&T> {
    self.vec.last()
  }

  #[inline(always)]
  pub fn last_mut(&mut self) -> Option<&mut T> {
    self.vec.last_mut()
  }

  #[inline(always)]
  pub fn pop(&mut self) -> Option<T> {
    self.vec.pop()
  }

  pub fn get2_mut(&mut self, a: I, b: I) -> Option<(&mut T, &mut T)> {
    let a = a.into();
    let b = b.into();
    if a == b || a >= self.len() || b >= self.len() {
      None
    } else {
      Some(if a < b {
        let (l, r) = self.vec.split_at_mut(b);
        (&mut l[a], &mut r[0])
      } else {
        let (l, r) = self.vec.split_at_mut(a);
        (&mut r[0], &mut l[b])
      })
    }
  }

  #[inline(always)]
  pub fn iter(&self) -> Iter<I, T> {
    self.into_iter()
  }

  #[inline(always)]
  pub fn iter_mut(&mut self) -> IterMut<I, T> {
    self.into_iter()
  }

  #[inline(always)]
  pub fn slice(&self, range: Range<I>) -> slice::Iter<T> {
    self.vec[range.start.into()..range.end.into()].iter()
  }

  #[inline(always)]
  pub fn slice_mut(&mut self, range: Range<I>) -> slice::IterMut<T> {
    self.vec[range.start.into()..range.end.into()].iter_mut()
  }

  #[inline(always)]
  pub fn keys(&self) -> impl Iterator<Item = I> + Clone {
    (0..self.vec.len()).map(I::from)
  }

  #[inline(always)]
  pub fn keys_from(&self, index: I) -> impl Iterator<Item = I> + Clone {
    (index..I::from(self.vec.len())).iter()
  }

  #[inline(always)]
  pub fn into_values(self) -> impl Iterator<Item = T> {
    self.vec.into_iter()
  }

  #[inline(always)]
  pub fn values(&self) -> impl Iterator<Item = &T> {
    self.vec.iter()
  }

  #[inline(always)]
  pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
    self.vec.iter_mut()
  }

  pub fn get_or_extend(&mut self, index: I) -> &mut T
  where
    T: Default,
  {
    self.get_or_extend_with(index, T::default)
  }

  pub fn get_or_extend_with(&mut self, index: I, f: impl FnMut() -> T) -> &mut T {
    let index: usize = index.into();
    if index >= self.vec.len() {
      self.vec.resize_with(index + 1, f);
    }
    &mut self.vec[index]
  }

  pub fn truncate(&mut self, len: usize) {
    self.vec.truncate(len);
  }

  pub fn clear(&mut self) {
    self.vec.clear();
  }

  pub fn drain(&mut self) -> Map<Enumerate<vec::Drain<T>>, MapEntry<I, T>> {
    self.vec.drain(..).enumerate().map(map_entry)
  }

  pub fn fill(&mut self, value: T)
  where
    T: Clone,
  {
    self.vec.fill(value);
  }
}

impl<I: Idx, T> Index<I> for IdxVec<I, T> {
  type Output = T;

  fn index(&self, index: I) -> &T {
    &self.vec[index.into()]
  }
}

impl<I: Idx, T> IndexMut<I> for IdxVec<I, T> {
  fn index_mut(&mut self, index: I) -> &mut T {
    &mut self.vec[index.into()]
  }
}

impl<I: Idx + Debug, T: Debug> Debug for IdxVec<I, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut f = f.debug_map();
    f.entries(self.iter());
    f.finish()
  }
}

impl<I: Idx, T> From<Vec<T>> for IdxVec<I, T> {
  fn from(vec: Vec<T>) -> Self {
    IdxVec { vec, PhantomData }
  }
}

impl<I: Idx, T> From<IdxVec<I, T>> for Vec<T> {
  fn from(value: IdxVec<I, T>) -> Self {
    value.vec
  }
}

impl<I: Idx, T, const N: usize> From<[T; N]> for IdxVec<I, T> {
  fn from(value: [T; N]) -> Self {
    Self::from(Vec::from(value))
  }
}

impl<I: Idx, T> Default for IdxVec<I, T> {
  fn default() -> Self {
    Self { vec: Default::default(), PhantomData }
  }
}

fn map_entry<I: Idx, T>((index, value): (usize, T)) -> (I, T) {
  (index.into(), value)
}

type MapEntry<I, T> = fn((usize, T)) -> (I, T);
pub type IntoIter<I, T> = Map<Enumerate<vec::IntoIter<T>>, MapEntry<I, T>>;
pub type Iter<'a, I, T> = Map<Enumerate<slice::Iter<'a, T>>, MapEntry<I, &'a T>>;
pub type IterMut<'a, I, T> = Map<Enumerate<slice::IterMut<'a, T>>, MapEntry<I, &'a mut T>>;

impl<I: Idx, T> IntoIterator for IdxVec<I, T> {
  type Item = (I, T);
  type IntoIter = IntoIter<I, T>;
  fn into_iter(self) -> Self::IntoIter {
    self.vec.into_iter().enumerate().map(map_entry)
  }
}

impl<'a, I: Idx, T> IntoIterator for &'a IdxVec<I, T> {
  type Item = (I, &'a T);
  type IntoIter = Iter<'a, I, T>;
  fn into_iter(self) -> Self::IntoIter {
    self.vec.iter().enumerate().map(map_entry)
  }
}

impl<'a, I: Idx, T> IntoIterator for &'a mut IdxVec<I, T> {
  type Item = (I, &'a mut T);
  type IntoIter = IterMut<'a, I, T>;
  fn into_iter(self) -> Self::IntoIter {
    self.vec.iter_mut().enumerate().map(map_entry)
  }
}

pub type RangeIter<I> = Map<Range<usize>, fn(usize) -> I>;
pub trait RangeExt<I: Idx> {
  fn iter(&self) -> RangeIter<I>;
  fn get(&self, i: usize) -> I;
}

impl<I: Idx> RangeExt<I> for Range<I> {
  fn iter(&self) -> RangeIter<I> {
    (self.start.into()..self.end.into()).map(I::from)
  }

  fn get(&self, i: usize) -> I {
    let el = I::from(self.start.into() + i);
    assert!(self.contains(&el));
    el
  }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Counter<I: Idx>(pub I);

impl<I: Idx> Counter<I> {
  #[allow(clippy::should_implement_trait)]
  #[inline(always)]
  pub fn next(&mut self) -> I {
    let cur = self.0;
    self.0 = I::from(cur.into() + 1);
    cur
  }

  pub fn chunk(&mut self, len: usize) -> Range<I> {
    let end = I::from(self.0.into() + len);
    let range = self.0..end;
    self.0 = end;
    range
  }

  pub fn peek_next(&self) -> I {
    self.0
  }

  pub fn count(&self) -> usize {
    self.0.into()
  }

  pub fn reset(&mut self) {
    self.0 = I::default();
  }
}
