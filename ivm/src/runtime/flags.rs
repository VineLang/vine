/// Error flags set during interactions.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Flags {
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
    let Self { no_io, vicious, ext_copy, ext_erase, ext_generic, invalid_interaction } = self;
    let mut errors = Vec::new();

    if *no_io {
      errors.push("Error: the net did not return its `IO` handle");
      if debug_hint {
        errors.push("  hint: try running the program in `--debug` mode to see error messages");
      }
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
