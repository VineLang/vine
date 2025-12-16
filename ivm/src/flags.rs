/// Error flags set during interactions.
#[derive(Default)]
pub struct Flags {
  /// A non-copyable extrinsic was copied.
  pub ext_copy: bool,
  /// A non-copyable extrinsic was erased.
  pub ext_erase: bool,
}
