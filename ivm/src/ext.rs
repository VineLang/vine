//! External values and functions, whose semantics live outside interaction
//! combinators.

#![allow(nonstandard_style)]

use core::{
  fmt::{self, Debug},
  marker::PhantomData,
};

use crate::{ivm::IVM, port::Tag, wire::Wire};

macro_rules! trait_alias {
  ($($(#[$attr:meta])* $vis:vis trait $name:ident[$($gen:tt)*] = ($($trait:tt)*);)*) => {$(
    $(#[$attr])*
    $vis trait $name<$($gen)*>: $($trait)* {}
    impl<$($gen)*, T> $name<$($gen)*> for T where T: $($trait)* {}
  )*};
}

trait_alias! {
  /// A trait alias for `ExtVal -> (ExtVal, ExtVal)` extrinsic functions.
  pub trait ExtSplitFn['ivm] = (
    for<'a, 'ext> Fn(&'a mut IVM<'ivm, 'ext>, ExtVal<'ivm>, Wire<'ivm>, Wire<'ivm>) + Send + Sync + 'ivm
  );

  /// A trait alias for `(ExtVal, ExtVal) -> ExtVal` extrinsic functions.
  pub trait ExtMergeFn['ivm] = (
    for<'a, 'ext> Fn(&'a mut IVM<'ivm, 'ext>, ExtVal<'ivm>, ExtVal<'ivm>, Wire<'ivm>) + Send + Sync + 'ivm
  );
}

pub struct Extrinsics<'ivm> {
  ext_split_fns: Vec<Box<dyn ExtSplitFn<'ivm>>>,
  ext_merge_fns: Vec<Box<dyn ExtMergeFn<'ivm>>>,

  /// Number of registered extrinsic types.
  ext_tys: u16,

  /// An always-present extrinsic type of n32 numbers.
  n32_ext_ty_id: ExtTyId<'ivm>,

  phantom: PhantomData<fn(&'ivm ()) -> &'ivm ()>,
}

impl Default for Extrinsics<'_> {
  fn default() -> Self {
    let n32_ext_ty_id = ExtTyId::new(0, true);

    Self {
      ext_split_fns: Vec::new(),
      ext_merge_fns: Vec::new(),
      ext_tys: 1,
      n32_ext_ty_id,
      phantom: PhantomData,
    }
  }
}

impl<'ivm> Extrinsics<'ivm> {
  pub const MAX_EXT_FN_COUNT: usize = 0x3FFF;
  pub const MAX_EXT_TY_COUNT: usize = 0x7FFF;

  pub fn new_split_ext_fn(&mut self, f: impl ExtSplitFn<'ivm>) -> ExtFn<'ivm> {
    if self.ext_split_fns.len() >= Self::MAX_EXT_FN_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic functions.");
    } else {
      let ext_fn = ExtFn::new_split(self.ext_split_fns.len());
      let f = Box::new(f) as Box<dyn ExtSplitFn<'ivm>>;
      self.ext_split_fns.push(f);
      ext_fn
    }
  }

  pub fn new_merge_ext_fn(&mut self, f: impl ExtMergeFn<'ivm>) -> ExtFn<'ivm> {
    if self.ext_merge_fns.len() >= Self::MAX_EXT_FN_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic functions.");
    } else {
      let ext_fn = ExtFn::new_merge(self.ext_merge_fns.len());
      let f = Box::new(f) as Box<dyn ExtMergeFn<'ivm>>;
      self.ext_merge_fns.push(f);
      ext_fn
    }
  }

  pub fn new_ext_ty<T>(&mut self, copy: bool) -> impl ExtTy<'ivm, T>
  where
    ExtTyId<'ivm>: ExtTy<'ivm, T>,
  {
    if self.ext_tys as usize >= Self::MAX_EXT_TY_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic unboxed types.");
    } else {
      self.ext_tys += 1;
      ExtTyId::new(self.ext_tys, copy)
    }
  }

  pub fn n32_ext_ty(&self) -> impl ExtTy<'ivm, u32> {
    self.n32_ext_ty_id
  }

  pub fn ext_val_as_n32(&self, x: ExtVal<'ivm>) -> u32 {
    self.n32_ext_ty().from_ext_val(x).unwrap()
  }

  pub fn get_ext_split_fn(&self, ext_fn: ExtFn<'ivm>) -> &dyn ExtSplitFn<'ivm> {
    self.ext_split_fns.get(ext_fn.index()).expect("unknown extrinsic function")
  }

  pub fn get_ext_merge_fn(&self, ext_fn: ExtFn<'ivm>) -> &dyn ExtMergeFn<'ivm> {
    self.ext_merge_fns.get(ext_fn.index()).expect("unknown extrinsic function")
  }
}

/// An external value with a 45-bit payload.
///
/// The 64-bit value is laid out as follows:
/// - top 16 bits are the extrinsic's type id [`ExtTyId`].
/// - the next 45 bits are the payload.
/// - the last 3 bits are `Tag::ExtVal as u16` for compatibility with [`Port`].
///
/// [`Port`]: crate::ivm::Port
#[derive(Clone, Copy)]
pub struct ExtVal<'ivm>(u64, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtVal<'ivm> {
  const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFF8;

  /// Creates an `ExtVal` from the raw bits.
  ///
  /// ## Safety
  /// The bits must be a valid representation of an `ExtVal`.
  #[inline(always)]
  pub unsafe fn from_bits(bits: u64) -> Self {
    Self(bits, PhantomData)
  }

  /// Returns the raw bits of this value.
  #[inline(always)]
  pub fn bits(&self) -> u64 {
    self.0
  }

  /// Creates a new `ExtVal` with a given type and payload.
  #[inline(always)]
  pub fn new(ty_id: ExtTyId<'ivm>, payload: u64) -> Self {
    debug_assert!(
      payload & !(Self::PAYLOAD_MASK >> 3) == 0,
      "ExtVal::new with non-payload bits set"
    );

    unsafe {
      Self::from_bits(
        ((ty_id.bits() as u64) << 48)
          | ((payload << 3) & Self::PAYLOAD_MASK)
          | (Tag::ExtVal as u64),
      )
    }
  }

  #[inline(always)]
  pub fn ty_id(&self) -> ExtTyId<'ivm> {
    ExtTyId::from_bits((self.0 >> 48) as u16)
  }

  #[inline(always)]
  fn payload(&self) -> u64 {
    (self.0 & Self::PAYLOAD_MASK) >> 3
  }

  /// Interprets this extrinsic value as a `T` through [`ExtTy<'ivm, T>`].
  #[inline(always)]
  pub fn as_ty<T>(self) -> T
  where
    ExtTyId<'ivm>: ExtTy<'ivm, T>,
  {
    // this never returns `None` since self.ty_id() == self.ty_id()
    ExtTy::from_ext_val(self.ty_id(), self).unwrap()
  }
}

impl<'ivm> Debug for ExtVal<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ExtVal({:?})", self.0)
  }
}

/// A trait to be implemented for `ExtTyId<'ivm>` when a Rust type `T` can be
/// treated as an IVM extrinsic type.
#[allow(clippy::wrong_self_convention)]
pub trait ExtTy<'ivm, T>: 'ivm + Copy {
  fn from_ext_val(self, x: ExtVal<'ivm>) -> Option<T> {
    if self.ty_id() == x.ty_id() { Some(self.from_payload(x.payload())) } else { None }
  }

  fn into_ext_val(self, value: T) -> ExtVal<'ivm> {
    ExtVal::new(self.ty_id(), self.into_payload(value))
  }

  fn ty_id(self) -> ExtTyId<'ivm>;
  fn from_payload(self, payload: u64) -> T;
  fn into_payload(self, value: T) -> u64;
}

impl<'ivm> ExtTy<'ivm, u32> for ExtTyId<'ivm> {
  fn ty_id(self) -> ExtTyId<'ivm> {
    self
  }

  fn from_payload(self, payload: u64) -> u32 {
    payload as u32
  }

  fn into_payload(self, value: u32) -> u64 {
    value as u64
  }
}

impl<'ivm> ExtTy<'ivm, f32> for ExtTyId<'ivm> {
  fn ty_id(self) -> ExtTyId<'ivm> {
    self
  }

  fn from_payload(self, payload: u64) -> f32 {
    f32::from_bits(payload as u32)
  }

  fn into_payload(self, value: f32) -> u64 {
    value.to_bits() as u64
  }
}

impl<'ivm> ExtTy<'ivm, ()> for ExtTyId<'ivm> {
  fn ty_id(self) -> ExtTyId<'ivm> {
    self
  }

  fn from_payload(self, _payload: u64) {}

  fn into_payload(self, (): ()) -> u64 {
    0
  }
}

/// The type id of an external value.
///
/// The highest bit specifies whether it is copyable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtTyId<'ivm>(u16, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtTyId<'ivm> {
  const COPY_BIT: u16 = 0x8000;

  #[inline(always)]
  fn new(ty_id: u16, copy: bool) -> Self {
    Self::from_bits(ty_id | if copy { Self::COPY_BIT } else { 0 })
  }

  #[inline(always)]
  fn from_bits(bits: u16) -> Self {
    Self(bits, PhantomData)
  }

  #[inline(always)]
  fn bits(self) -> u16 {
    self.0
  }

  #[allow(unused)]
  #[inline(always)]
  fn is_copy(self) -> bool {
    self.0 & Self::COPY_BIT != 0
  }
}

/// A type uniquely identifying an extrinsic function.
///
/// The highest bit denotes whether the function arguments are swapped.
/// The middle 14 bits denote an index in one of `ext_split_fns` or
/// `ext_merge_fns`. The lowest bit denotes which of `ext_split_fns` or
/// `ext_merge_fns` the index applies to.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtFn<'ivm>(u16, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtFn<'ivm> {
  const SWAP_BIT: u16 = 0x8000;
  const MERGE_BIT: u16 = 0x4000;
  const ID_MASK: u16 = 0x3FFF;

  #[inline(always)]
  pub fn new_split(id: usize) -> Self {
    Self(id as u16, PhantomData)
  }

  #[inline(always)]
  pub fn new_merge(id: usize) -> Self {
    Self(id as u16 | Self::MERGE_BIT, PhantomData)
  }

  #[inline(always)]
  pub fn bits(&self) -> u16 {
    self.0
  }

  #[inline(always)]
  pub fn is_swapped(&self) -> bool {
    self.0 & Self::SWAP_BIT != 0
  }

  #[inline(always)]
  pub fn is_merge(&self) -> bool {
    self.0 & Self::MERGE_BIT != 0
  }

  #[inline(always)]
  pub fn index(&self) -> usize {
    (self.0 & Self::ID_MASK) as usize
  }

  #[inline(always)]
  pub fn swapped(&self) -> Self {
    Self(self.0 ^ Self::SWAP_BIT, PhantomData)
  }
}

impl<'ivm> Debug for ExtFn<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ExtFn({:?})", self.0)
  }
}

#[derive(Debug, Clone, Copy)]
pub struct OpaqueExtFn {
  pub label: u16,
  pub swap: bool,
}
