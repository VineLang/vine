use core::{
  fmt::{self, Debug},
  marker::PhantomData,
  mem::transmute,
  ops::Deref,
  ptr,
};

use crate::{
  addr::Addr,
  port::Port,
  word::{AtomicWord, Word},
};

/// A wire in the interaction net.
///
/// This is represented by a pointer to an `AtomicWord` that stores the *target*
/// of this wire, which is either a port, if one side of this wire has been
/// linked, or none, otherwise.
#[repr(transparent)]
pub struct Wire<'ivm>(Addr, PhantomData<&'ivm mut &'ivm ()>);

impl<'ivm> Wire<'ivm> {
  /// ## Safety
  /// `addr` must be a pointer to an `AtomicWord` value associated with `'ivm`.
  #[inline(always)]
  pub unsafe fn from_addr(addr: Addr) -> Self {
    Wire(addr, PhantomData)
  }

  fn target(&self) -> &'ivm AtomicWord {
    unsafe { self.addr().as_word() }
  }

  #[inline(always)]
  pub(crate) fn load_target(&self) -> Option<Port<'ivm>> {
    unsafe { Port::option_from_bits(self.target().load()) }
  }

  #[inline(always)]
  pub(crate) fn swap_target(&self, new: Option<Port<'ivm>>) -> Option<Port<'ivm>> {
    let old = self.target().swap(unsafe { transmute::<Option<Port>, Word>(new) });
    unsafe { Port::option_from_bits(old) }
  }

  #[inline(always)]
  pub(crate) fn addr(&self) -> Addr {
    self.0
  }

  /// Unsafely clones a wire by copying its bits.
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
}

impl<'ivm> Debug for Wire<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Wire({:?})", self.addr())
  }
}

/// Semantically analogous to `&'a Wire<'ivm>`.
pub struct WireRef<'a, 'ivm>(Wire<'ivm>, PhantomData<&'a ()>);

impl<'a, 'ivm> WireRef<'a, 'ivm> {
  pub(crate) fn from_wire(wire: Wire<'ivm>) -> Self {
    WireRef(wire, PhantomData)
  }
}

impl<'a, 'ivm> Deref for WireRef<'a, 'ivm> {
  type Target = Wire<'ivm>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<'a, 'ivm> From<&'a Wire<'ivm>> for WireRef<'a, 'ivm> {
  fn from(wire: &'a Wire<'ivm>) -> Self {
    Self::from_wire(unsafe { wire.clone() })
  }
}
