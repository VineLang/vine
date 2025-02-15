use crate::{addr::Addr, port::Port, word::Word, IVM};

/// A global definition of a net.
#[derive(Debug)]
pub struct Global<'ivm> {
  /// The name of this global; to be used for debugging and readback only.
  pub name: String,
  /// The set of all labels used in this net (including indirectly).
  pub labels: LabelSet,
  pub nodes: Vec<[Option<Port<'ivm>>; 2]>,
  pub pairs: Vec<[Option<Port<'ivm>>; 2]>,
  /// An internal implementation detail used in ivy's serialization logic.
  #[doc(hidden)]
  pub flag: usize,
}

impl<'ivm> IVM<'ivm> {
  pub fn instantiate(&mut self, root: Port<'ivm>, global: &Global<'ivm>) {
    if global.nodes.len() > self.mapping.len() {
      self.mapping.resize(global.nodes.len(), Addr::NULL);
    }

    for i in 0..global.nodes.len() {
      unsafe { *self.mapping.get_unchecked_mut(i) = self.alloc_node() };
    }

    unsafe {
      for i in 0..global.nodes.len() {
        if let Some(port) = &global.nodes[i][0] {
          (*self.mapping.get_unchecked(i)).as_word().store(self.map_port(port).to_bits());
        }
        if let Some(port) = &global.nodes[i][1] {
          (*self.mapping.get_unchecked(i))
            .other_half()
            .as_word()
            .store(self.map_port(port).to_bits());
        }
      }
    }

    for [a, b] in &global.pairs {
      let a = a.as_ref().map(|x| self.map_port(x)).unwrap_or(unsafe { root.clone() });
      let b = b.as_ref().map(|x| self.map_port(x)).unwrap_or(unsafe { root.clone() });
      self.active.push((a, b));
    }
  }

  fn map_port(&mut self, port: &Port<'ivm>) -> Port<'ivm> {
    if (port.tag() as u8) & 1 == 1 {
      unsafe {
        let p = port.to_bits().bits();
        let addr = *self.mapping.get_unchecked(((p as u32) >> 4) as usize);
        Port::from_bits(Word::from_ptr(addr.0).map_bits(|a| p & 0xFFFF_0000_0000_000F | a))
      }
    } else {
      unsafe { port.clone() }
    }
  }
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
