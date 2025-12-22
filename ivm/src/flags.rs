/// Error flags set during interactions.
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Flags {
  /// A non-copyable extrinsic was copied.
  pub ext_copy: bool,
  /// A non-copyable extrinsic was erased.
  pub ext_erase: bool,
}

impl Flags {
  pub fn success(self) -> bool {
    self == Self::default()
  }
}
