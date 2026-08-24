use alloc::{string::String, vec::Vec};

/// Error flags set during interactions.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Flags {
  pub error: bool,
  pub no_io: bool,
  pub vicious: bool,
  pub ext_copy: bool,
  pub ext_erase: bool,
  pub ext_generic: bool,
  pub invalid_interaction: bool,
}

impl Flags {
  pub fn success(self) -> bool {
    self == Self::default()
  }

  pub fn error_message(&self, debug_hint: bool) -> String {
    let Self { error, no_io, vicious, ext_copy, ext_erase, ext_generic, invalid_interaction } =
      self;
    let mut errors = Vec::new();

    if *error {
      errors.push("Error: a runtime error occurred");
      if debug_hint {
        errors.push("  hint: try running the program in `--debug` mode to see error messages");
      }
    }
    if *no_io {
      errors.push("Error: the net did not return its `IO` handle");
    }
    if *vicious {
      errors.push("Error: the net created a vicious circle");
    }
    if *ext_copy {
      errors.push("Error: a linear extrinsic was copied");
    }
    if *ext_erase {
      errors.push("Error: a linear extrinsic was erased");
    }
    if *ext_generic {
      errors.push("Error: an extrinsic function encountered an unspecified error");
    }
    if *invalid_interaction {
      errors.push("Error: an invalid interaction occurred");
    }

    errors.join("\n\n")
  }
}
