use std::ops::{Add, Range};

use crate::{idx::IdxVec, new_idx};

/// Stores a tree of partial sums of an array, allowing:
/// - calculating the sum of any subarray in logarithmic time
/// - updating the value of an element in the array in logarithmic time
///
/// Addition on `T`s must be associative with an identity of `Default`, but it
/// need not be commutative or invertible.
#[derive(Debug)]
pub struct SumTree<T> {
  sums: IdxVec<Subtree, T>,
}

impl<T: Default + Add<T, Output = T> + Clone> SumTree<T> {
  pub fn new(values: impl Iterator<Item = T>) -> SumTree<T> {
    let mut sums = IdxVec::new();
    for (i, value) in values.into_iter().enumerate() {
      if i != 0 {
        _ = sums.push(T::default());
      }
      _ = sums.push(value);
      let mut cur = Subtree::leaf(i);
      for _ in 0..i.trailing_ones() {
        let (left, parent, right) = cur.family();
        sums[parent] = sums[left].clone() + sums[right].clone();
        cur = parent;
      }
    }
    SumTree { sums }
  }

  pub fn update(&mut self, i: usize, value: T) {
    let mut cur = Subtree::leaf(i);
    self.sums[cur] = value;
    loop {
      let (left, parent, right) = cur.family();
      if right >= self.sums.next_index() {
        break;
      }
      self.sums[parent] = self.sums[left].clone() + self.sums[right].clone();
      cur = parent;
    }
  }

  pub fn sum(&self, Range { start, end }: Range<usize>) -> T {
    if end <= start {
      return T::default();
    }

    let end = end - 1;

    if start == end {
      return self.sums[Subtree::leaf(start)].clone();
    }

    let mut prefix = T::default();
    let mut subtree = Subtree::leaf(start);
    while subtree.end() < end {
      prefix = prefix + self.sums[subtree].clone();
      subtree = subtree.next();
    }

    let mut suffix = T::default();
    let mut subtree = Subtree::leaf(end);
    while start < subtree.start() {
      suffix = self.sums[subtree].clone() + suffix;
      subtree = subtree.prev();
    }

    prefix + suffix
  }
}

new_idx!(Subtree);

impl Subtree {
  fn leaf(i: usize) -> Subtree {
    Subtree(i << 1)
  }

  fn degree(self) -> u32 {
    self.0.trailing_ones()
  }

  fn start(self) -> usize {
    (self.0 - ((1 << self.degree()) - 1)) >> 1
  }

  fn end(self) -> usize {
    (self.0 + ((1 << self.degree()) - 1)) >> 1
  }

  fn next(mut self) -> Subtree {
    self.0 += 1 << self.degree();
    self.0 += 1 << (self.degree() - 1);
    self
  }

  fn prev(mut self) -> Subtree {
    self.0 -= 1 << self.degree();
    self.0 -= 1 << (self.degree() - 1);
    self
  }

  fn family(self) -> (Subtree, Subtree, Subtree) {
    (
      Subtree(self.0 & !(2 << self.degree())),
      Subtree(self.0 & !(2 << self.degree()) | (1 << self.degree())),
      Subtree(self.0 | (2 << self.degree())),
    )
  }
}

#[test]
fn test() {
  #[derive(Debug, Default, Clone, PartialEq, Eq)]
  struct El(Vec<usize>);
  impl Add for El {
    type Output = El;
    fn add(mut self, mut rhs: Self) -> Self::Output {
      self.0.append(&mut rhs.0);
      self
    }
  }

  #[derive(Debug)]
  struct Reference<T>(Vec<T>);
  impl<T: Default + Add<T, Output = T> + Clone> Reference<T> {
    fn new(values: impl IntoIterator<Item = T>) -> Self {
      Reference(values.into_iter().collect())
    }

    fn update(&mut self, i: usize, value: T) {
      self.0[i] = value;
    }

    fn sum(&self, range: Range<usize>) -> T {
      let mut sum = T::default();
      for value in &self.0[range] {
        sum = sum + value.clone();
      }
      sum
    }
  }

  fn check(actual: &SumTree<El>, reference: &Reference<El>, len: usize) {
    for start in 0..len {
      for end in start..len {
        assert_eq!(actual.sum(start..end), reference.sum(start..end));
      }
    }
  }

  for len in 0..16 {
    let mut actual = SumTree::new((0..len).map(|x| El(vec![x])));
    let mut reference = Reference::new((0..len).map(|x| El(vec![x])));
    check(&actual, &reference, len);
    for i in 0..len {
      actual.update(i, El(vec![i + len]));
      reference.update(i, El(vec![i + len]));
      check(&actual, &reference, len);
    }
  }
}
