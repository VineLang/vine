/// Expands to an enum implementing [`Iterator`] with N variants and N generic
/// parameters.
///
/// This is a similar to [`itertools::Either`], but supports a variable number
/// of variants, and can be used effectively with an opaque return type:
/// ```rust
/// use vine_util::multi_iter;
///
/// fn numbers(num: &'static str) -> impl Iterator<Item = usize> {
///   multi_iter! { Numbers { Zero, One, Two } }
///   match num {
///     "one" => Numbers::One([1]),
///     "two" => Numbers::Two([1, 2]),
///     _ => Numbers::Zero(std::iter::empty()),
///   }
/// }
/// ```
///
/// [`itertools::Either`]: https://docs.rs/itertools/latest/itertools/enum.Either.html
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
