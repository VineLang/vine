use crate::instruction::Instructions;

/// A global definition of a net.
#[derive(Debug, Default)]
pub struct Global<'ivm> {
  /// The name of this global; to be used for debugging and readback only.
  pub name: String,
  /// The set of all labels used in this net (including indirectly).
  pub labels: LabelSet,
  /// The instructions for constructing this net.
  pub instructions: Instructions<'ivm>,
  /// An internal implementation detail used in ivy's serialization logic.
  #[doc(hidden)]
  pub flag: usize,
}

/// A set of labels used in a global.
///
/// Because of the representation used here, `self.has(u16::MAX)` will always
/// return false, even if `self.add(u16::MAX)` is called.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LabelSet {
  /// The least label greater than all labels in this set.
  cap: u16,
  /// A bitset, containing at least `cap` bits (equivalently, at least `ceil(cap
  /// / 64)` entries).
  bits: Vec<u64>,
}

impl LabelSet {
  /// The empty set.
  pub const NONE: Self = Self { cap: 0, bits: Vec::new() };

  /// Inserts a label into this set.
  #[inline]
  pub fn add(&mut self, label: u16) {
    if label == u16::MAX {
      return;
    }
    self.cap = self.cap.max(label + 1);
    let index = (label >> 6) as usize;
    let bit = label & 63;
    if index >= self.bits.len() {
      self.bits.resize(index + 1, 0);
    }
    self.bits[index] |= 1 << bit;
  }

  /// Returns whether `label` is in this set.
  #[inline(always)]
  pub fn has(&self, label: u16) -> bool {
    if label >= self.cap {
      return false;
    }
    let index = (label >> 6) as usize;
    let bit = label & 63;
    unsafe { self.bits.get_unchecked(index) & 1 << bit != 0 }
  }

  /// Inserts all labels in `other` into this set.
  #[inline]
  pub fn union(&mut self, other: &LabelSet) {
    self.cap = self.cap.max(other.cap);
    for (a, b) in self.bits.iter_mut().zip(other.bits.iter()) {
      *a |= b;
    }
    if other.bits.len() > self.bits.len() {
      self.bits.extend_from_slice(&other.bits[self.bits.len()..])
    }
  }
}
