use super::*;

/// A textual comment, possibly spanning multiple lines.
#[derive_(Default, Debug, Clone, Visit!)]
pub struct Text {
  pub lines: Vec<String>,
}

/// A line or block exclusion comment.
#[derive_(Default, Debug, Clone, Visit!)]
pub struct Exclusion {
  pub block: bool,
  /// Should be non-empty. If `block` is true, should have exactly one
  /// element.
  pub lines: Vec<String>,
}
