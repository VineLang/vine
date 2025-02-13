use crate::{
  addr::Addr,
  ext::{ExtFn, ExtVal},
  global::Global,
  wire::{Wire, WireRef},
  word::{NonZeroWord, Word},
};
use core::{
  fmt::{self, Debug},
  marker::PhantomData,
  mem::transmute,
  ops::Deref,
  ptr,
};

/// A port in the interaction net.
///
/// The interpretation of a port is determined by its *tag*, stored in the
/// bottom three bits, and accessible with [`Port::tag`].
///
/// All tags other than `ExtVal` divide the remaining bits of a port as follows:
/// - the top 16 bits are its *label*, accessible with [`Port::label`]
/// - the next 45 bits are the non-alignment bits of its *address*, a pointer
///   aligned to 8 bytes, accessible with [`Port::addr`]
/// - the bottom 3 bits are its tag, as always
///
/// The interpretation of these fields once again depends on the tag; see each
/// tag's documentation for details.
#[repr(transparent)]
pub struct Port<'ivm>(NonZeroWord, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

/// The 3-bit tag of a [`Port`], determining its interpretation.
///
/// Bit pattern `0b000` is invalid and used by various systems to represent
/// non-port values.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tag {
  /// This port is a wrapper around a [`Wire`]. Its address is that of the wire,
  /// and its label is zero.
  Wire = 1,

  /// This port is a reference to a global net. Its address is an
  /// `&'ivm`[`Global`].
  Global = 2,

  /// This port is the principal port of an eraser node. Its address and label
  /// are both zero.
  Erase = 3,

  /// This port is a wrapper around an [`ExtVal`]. Its address and label are
  /// semantically irrelevant; `ExtVal` uses all the non-tag bits
  /// indiscriminantly.
  ExtVal = 4,

  /// This port is the principal port of a binary combinator. Its address points
  /// to a node in the heap, and its label is the label of the combinator.
  Comb = 5,

  /// This port is the principal port of a binary external function node. Its
  /// address points to a node in the heap, and its label is the [`ExtFn`] of
  /// this node.
  ExtFn = 6,

  /// This port is the principal port of a binary branch node. Its address
  /// points to a node in the heap, and its label is zero.
  ///
  /// Though branch nodes are semantically trinary, at runtime the trinary node
  /// `?(a b c)` is represented with two binary nodes: `?(?(a b) c)`.
  Branch = 7,
}

impl<'ivm> Port<'ivm> {
  /// The principal port of an eraser node.
  pub const ERASE: Self = unsafe { Port::from_bits(Word::from_bits(Tag::Erase as u64)) };

  /// Constructs a port from its bit representation.
  ///
  /// ## Safety
  /// The bits of `word` must comply with the restrictions of a `Port`.
  #[inline(always)]
  pub(crate) const unsafe fn from_bits(word: Word) -> Self {
    Self(NonZeroWord::new_unchecked(word), PhantomData)
  }

  /// Constructs a port from its bit representation, if its tag is valid.
  ///
  /// ## Safety
  /// The bits of `word` must either comply with the restrictions of a `Port`,
  /// or have an invalid tag.
  #[inline(always)]
  pub(crate) unsafe fn option_from_bits(word: Word) -> Option<Self> {
    if word.bits() & 0b111 == 0 {
      None
    } else {
      Some(Self::from_bits(word))
    }
  }

  /// Constructs a new port from its components.
  ///
  /// ## Safety
  /// `label` and `addr` must comply with the requirements of `tag`.
  #[inline(always)]
  pub unsafe fn new(tag: Tag, label: u16, addr: Addr) -> Self {
    let port = Port::from_bits(
      Word::from_ptr(addr.0).map_bits(|addr| label as (u64) << 48 | addr | tag as u64),
    );
    debug_assert_eq!(port.addr(), addr);
    port
  }

  /// Accesses the tag of this port.
  #[inline(always)]
  pub fn tag(&self) -> Tag {
    unsafe { transmute::<u8, Tag>((self.0.get().bits() & 0b111) as u8) }
  }

  /// Accesses the label of this port. The interpretation of this value depends
  /// on the tag.
  #[inline(always)]
  pub fn label(&self) -> u16 {
    (self.0.get().bits() >> 48) as u16
  }

  /// Accesses the address of this port. The interpretation of this value
  /// depends on the tag.
  #[inline(always)]
  pub fn addr(&self) -> Addr {
    Addr(self.0.get().map_bits(|x| x & 0x0000_FFFF_FFFF_FFF8).ptr())
  }

  /// Constructs a new [`Tag::Global`] port.
  #[inline(always)]
  pub fn new_global(global: &'ivm Global) -> Self {
    unsafe { Self::new(Tag::Global, 0, Addr(global as *const Global as *const ())) }
  }

  /// Constructs a new [`Tag::Wire`] port.
  #[inline(always)]
  pub fn new_wire(wire: Wire<'ivm>) -> Self {
    unsafe { Self::new(Tag::Wire, 0, wire.addr()) }
  }

  /// Constructs a new [`Tag::ExtVal`] port.
  #[inline(always)]
  pub fn new_ext_val(val: ExtVal) -> Self {
    unsafe { Self::from_bits(Word::from_bits(val.bits())) }
  }

  /// Unsafely clones a port by copying its bits.
  ///
  /// ## Safety
  /// At most one clone may be linked.
  ///
  /// This can't currently cause UB (just severe logic errors), but may in the
  /// future.
  #[inline(always)]
  pub unsafe fn clone(&self) -> Self {
    unsafe { ptr::read(self) }
  }

  /// Get the label of this port, interpreted as an [`ExtFn`].
  ///
  /// ## Safety
  /// This port must be [`Tag::ExtFn`].
  #[inline(always)]
  pub unsafe fn as_ext_fn(&self) -> ExtFn<'ivm> {
    debug_assert_eq!(self.tag(), Tag::ExtFn);
    transmute::<u16, ExtFn>(self.label())
  }

  /// Get the address of this port, interpreted as an [`Wire`].
  ///
  /// ## Safety
  /// This port must be [`Tag::Wire`].
  #[inline(always)]
  pub unsafe fn as_wire(self) -> Wire<'ivm> {
    debug_assert_eq!(self.tag(), Tag::Wire);
    Wire::from_addr(self.addr())
  }

  /// Get the value of this port, interpreted as an [`ExtVal`].
  ///
  /// ## Safety
  /// This port must be [`Tag::ExtVal`].
  #[inline(always)]
  pub unsafe fn as_ext_val(&self) -> ExtVal<'ivm> {
    debug_assert_eq!(self.tag(), Tag::ExtVal);
    unsafe { ExtVal::from_bits(self.0.get().bits()) }
  }

  /// Get the address of this port, interpreted as an [`ExtVal`].
  ///
  /// ## Safety
  /// This port must be [`Tag::ExtVal`].
  #[inline(always)]
  pub unsafe fn as_global(&self) -> &'ivm Global<'ivm> {
    debug_assert_eq!(self.tag(), Tag::Global);
    self.addr().as_global()
  }

  /// Get the wires leaving the aux ports of this node.
  ///
  /// ## Safety
  /// This port must be the principal port of a binary node: [`Tag::Comb`],
  /// [`Tag::ExtVal`], or [`Tag::Branch`].
  #[inline(always)]
  pub unsafe fn aux(self) -> (Wire<'ivm>, Wire<'ivm>) {
    (Wire::from_addr(self.addr()), Wire::from_addr(self.addr().other_half()))
  }

  /// Get the wires leaving the aux ports of this node.
  ///
  /// ## Safety
  /// This port must be the principal port of a binary node: [`Tag::Comb`],
  /// [`Tag::ExtVal`], or [`Tag::Branch`].
  #[inline(always)]
  pub unsafe fn aux_ref(&self) -> (WireRef<'_, 'ivm>, WireRef<'_, 'ivm>) {
    (
      WireRef::from_wire(Wire::from_addr(self.addr())),
      WireRef::from_wire(Wire::from_addr(self.addr().other_half())),
    )
  }
}

impl<'ivm> Debug for Port<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.tag() {
      Tag::Wire => write!(f, "{:?}", unsafe { self.clone().as_wire() }),
      Tag::Global => write!(f, "Global({})", unsafe { self.as_global() }.name),
      Tag::Comb => write!(f, "Comb({}, {:?})", self.label(), self.addr()),
      Tag::Erase => write!(f, "Erase"),
      Tag::ExtVal => write!(f, "ExtVal({:?})", unsafe { self.as_ext_val() }),
      Tag::ExtFn => write!(f, "ExtFn({:?}, {:?})", unsafe { self.as_ext_fn() }, self.addr()),
      Tag::Branch => write!(f, "Branch({:?})", self.addr()),
    }
  }
}

/// Semantically analogous to `&'a Port<'ivm>`.
pub struct PortRef<'a, 'ivm>(Port<'ivm>, PhantomData<&'a ()>);

impl<'a, 'ivm> PortRef<'a, 'ivm> {
  pub(crate) fn from_port(port: Port<'ivm>) -> Self {
    PortRef(port, PhantomData)
  }

  #[inline(always)]
  pub fn new_wire(wire: &Wire<'ivm>) -> Self {
    unsafe { PortRef::from_port(Port::new_wire(wire.clone())) }
  }
}

impl<'a, 'ivm> Deref for PortRef<'a, 'ivm> {
  type Target = Port<'ivm>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<'a, 'ivm> From<&'a Port<'ivm>> for PortRef<'a, 'ivm> {
  fn from(port: &'a Port<'ivm>) -> Self {
    Self(unsafe { port.clone() }, PhantomData)
  }
}
