pub fn exact_size<T, const N: usize>(mut iter: impl ExactSizeIterator<Item = T>) -> [T; N] {
  assert_eq!(iter.len(), N);
  [(); N].map(|()| iter.next().unwrap())
}
