use core::{
  fmt::{self, Debug},
  marker::PhantomData,
  mem::transmute,
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
}

impl<'ivm> Debug for Wire<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Wire({:?})", self.addr())
  }
}
