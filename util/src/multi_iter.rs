#[macro_export]
macro_rules! multi_iter {
  ($Iter:ident { $($Variant:ident),* $(,)? }) => {
    #[derive(Debug, Clone, Copy)]
    enum $Iter<$($Variant),*> { $($Variant { iter: $Variant }),* }

    #[allow(non_snake_case)]
    impl<$($Variant),*> $Iter<$($Variant),*> {
      $(fn $Variant(iter: impl IntoIterator<IntoIter = $Variant>) -> Self {
        Self::$Variant { iter: iter.into_iter() }
      })*
    }

    impl<I, $($Variant: Iterator<Item = I>),*> Iterator for $Iter<$($Variant),*> {
      type Item = I;

      fn next(&mut self) -> Option<Self::Item> {
        match self {
          $(Self::$Variant { iter } => iter.next()),*
        }
      }

      fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
          $(Self::$Variant { iter } => iter.size_hint()),*
        }
      }
    }

    impl<I, $($Variant: DoubleEndedIterator<Item = I>),*> DoubleEndedIterator for $Iter<$($Variant),*> {
      fn next_back(&mut self) -> Option<Self::Item> {
        match self {
          $(Self::$Variant { iter } => iter.next_back()),*
        }
      }
    }

    impl<I, $($Variant: ExactSizeIterator<Item = I>),*> ExactSizeIterator for $Iter<$($Variant),*> {}
  };
}
