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
  /// An in-order representation of a binary tree, with array values at the
  /// leaf nodes, and sums of all contained leaves at each branch node.
  sums: IdxVec<Node, T>,
}

impl<T: Default + Add<T, Output = T> + Clone> SumTree<T> {
  pub fn new(values: impl Iterator<Item = T>) -> SumTree<T> {
    let mut sums = IdxVec::new();
    for (i, value) in values.into_iter().enumerate() {
      if i != 0 {
        _ = sums.push(T::default());
      }
      _ = sums.push(value);
      let mut cur = Node::leaf(i);
      for _ in 0..i.trailing_ones() {
        let (left, parent, right) = cur.family();
        sums[parent] = sums[left].clone() + sums[right].clone();
        cur = parent;
      }
    }
    SumTree { sums }
  }

  pub fn sum(&self, Range { start, end }: Range<usize>) -> T {
    if end <= start {
      return T::default();
    }

    let end = end - 1;

    if start == end {
      return self.sums[Node::leaf(start)].clone();
    }

    let mut prefix = T::default();
    let mut subtree = Node::leaf(start);
    while subtree.end() < end {
      prefix = prefix + self.sums[subtree].clone();
      subtree = subtree.next();
    }

    let mut suffix = T::default();
    let mut subtree = Node::leaf(end);
    while start < subtree.start() {
      suffix = self.sums[subtree].clone() + suffix;
      subtree = subtree.prev();
    }

    prefix + suffix
  }

  pub fn update(&mut self, i: usize, value: T) {
    let mut cur = Node::leaf(i);
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
}

// An index into the in-order representation of a binary tree.
new_idx!(Node);

impl Node {
  fn leaf(i: usize) -> Node {
    Node(i << 1)
  }

  fn degree(self) -> u32 {
    self.0.trailing_ones()
  }

  /// The index of the first leaf contained in this subtree.
  fn start(self) -> usize {
    (self.0 - ((1 << self.degree()) - 1)) >> 1
  }

  /// The index of the last leaf contained in this subtree.
  fn end(self) -> usize {
    (self.0 + ((1 << self.degree()) - 1)) >> 1
  }

  /// The root node of the largest subtree immediately to the right of this
  /// subtree.
  fn next(mut self) -> Node {
    self.0 += 1 << self.degree();
    self.0 += 1 << (self.degree() - 1);
    self
  }

  /// The root node of the largest subtree immediately to the left of this
  /// subtree.
  fn prev(mut self) -> Node {
    self.0 -= 1 << self.degree();
    self.0 -= 1 << (self.degree() - 1);
    self
  }

  /// Returns `(left, parent, right)`, where `left` and `right` are the children
  /// of `parent`, and `parent` is the parent of this node.
  fn family(self) -> (Node, Node, Node) {
    (
      Node(self.0 & !(2 << self.degree())),
      Node(self.0 & !(2 << self.degree()) | (1 << self.degree())),
      Node(self.0 | (2 << self.degree())),
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
