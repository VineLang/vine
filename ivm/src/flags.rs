/// Error flags set during interactions.
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Flags {
  /// A non-copyable extrinsic was copied.
  pub ext_copy: bool,
  /// A non-copyable extrinsic was erased.
  pub ext_erase: bool,
  /// An extrinsic function encountered an unspecified error.
  pub ext_generic: bool,
}

impl Flags {
  pub fn success(self) -> bool {
    self == Self::default()
  }

  pub fn error_message(&self) -> String {
    let mut messages = Vec::new();
    if self.ext_copy {
      messages.push("Error: a linear extrinsic was copied");
    }
    if self.ext_erase {
      messages.push("Error: a linear extrinsic was erased");
    }
    if self.ext_generic {
      messages.push("Error: an extrinsic function encountered an unspecified error");
    }
    messages.join("\n\n")
  }
}
