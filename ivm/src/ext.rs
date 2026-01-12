//! External values and functions, whose semantics live outside interaction
//! combinators.

#![allow(nonstandard_style)]

use core::{
  fmt::{self, Debug},
  marker::PhantomData,
  ops::{Deref, DerefMut},
};

use crate::{ivm::IVM, port::Tag, wire::Wire, word::Word};

use std::vec::IntoIter;

macro_rules! trait_alias {
  ($($(#[$attr:meta])* $vis:vis trait $name:ident = ($($trait:tt)*);)*) => {$(
    $(#[$attr])*
    $vis trait $name<'ivm>: $($trait)* {}
    impl<'ivm, T> $name<'ivm> for T where T: $($trait)* {}
  )*};
}

trait_alias! {
  /// A trait alias for `ExtVal -> (ExtVal, ExtVal)` extrinsic functions.
  pub trait ExtSplitFn = (
    for<'a, 'ext> Fn(&'a mut IVM<'ivm, 'ext>, ExtVal<'ivm>, Wire<'ivm>, Wire<'ivm>) + Send + Sync + 'ivm
  );

  /// A trait alias for `(ExtVal, ExtVal) -> ExtVal` extrinsic functions.
  pub trait ExtMergeFn = (
    for<'a, 'ext> Fn(&'a mut IVM<'ivm, 'ext>, ExtVal<'ivm>, ExtVal<'ivm>, Wire<'ivm>) + Send + Sync + 'ivm
  );
}

pub struct Extrinsics<'ivm> {
  ext_split_fns: Vec<Box<dyn ExtSplitFn<'ivm>>>,
  ext_merge_fns: Vec<Box<dyn ExtMergeFn<'ivm>>>,

  /// Number of registered extrinsic types.
  ext_tys: u16,

  /// An always-present extrinsic type of n32 numbers.
  n32_ext_ty: ExtTy<'ivm, u32>,

  phantom: PhantomData<fn(&'ivm ()) -> &'ivm ()>,
}

impl Default for Extrinsics<'_> {
  fn default() -> Self {
    let n32_ext_ty = ExtTy::new_unchecked(ExtTyId::new(0, true));

    Self {
      ext_split_fns: Vec::new(),
      ext_merge_fns: Vec::new(),
      ext_tys: 1,
      n32_ext_ty,
      phantom: PhantomData,
    }
  }
}

impl<'ivm> Extrinsics<'ivm> {
  pub const MAX_EXT_FN_COUNT: usize = 0x3FFF;
  pub const MAX_EXT_TY_COUNT: usize = 0x7FFF;

  pub fn new_split_ext_fn(&mut self, f: impl ExtSplitFn<'ivm>) -> ExtFn<'ivm> {
    assert!(self.ext_split_fns.len() < Self::MAX_EXT_FN_COUNT);

    let ext_fn = ExtFn::new_split(self.ext_split_fns.len());
    let f = Box::new(f) as Box<dyn ExtSplitFn<'ivm>>;
    self.ext_split_fns.push(f);
    ext_fn
  }

  pub fn new_merge_ext_fn(&mut self, f: impl ExtMergeFn<'ivm>) -> ExtFn<'ivm> {
    assert!(self.ext_merge_fns.len() < Self::MAX_EXT_FN_COUNT);

    let ext_fn = ExtFn::new_merge(self.ext_merge_fns.len());
    let f = Box::new(f) as Box<dyn ExtMergeFn<'ivm>>;
    self.ext_merge_fns.push(f);
    ext_fn
  }

  pub fn new_ext_ty<T: ExtTyCast<'ivm>>(&mut self) -> ExtTy<'ivm, T> {
    assert!((self.ext_tys as usize) < Self::MAX_EXT_TY_COUNT);

    self.ext_tys += 1;
    ExtTy::new_unchecked(ExtTyId::new(self.ext_tys, T::COPY))
  }

  pub fn n32_ext_ty(&self) -> ExtTy<'ivm, u32> {
    self.n32_ext_ty
  }

  pub fn ext_val_as_n32(&self, x: ExtVal<'ivm>) -> Option<u32> {
    self.n32_ext_ty().unwrap_ext_val(x)
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
pub struct ExtVal<'ivm>(Word, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtVal<'ivm> {
  const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFF8;

  /// Creates an `ExtVal` from the raw bits.
  ///
  /// ## Safety
  /// The bits must be a valid representation of an `ExtVal`.
  #[inline(always)]
  pub unsafe fn from_bits(bits: Word) -> Self {
    Self(bits, PhantomData)
  }

  /// Returns the raw bits of this value.
  #[inline(always)]
  pub fn bits(&self) -> Word {
    self.0
  }

  /// Creates a new `ExtVal` with a given type and 45-bit payload.
  ///
  /// The provided 45-bit payload can only occupy bits 48..3 (inclusive) of the
  /// provided `u64` value. That is, the highest 16 bits and lowest 3 bits must
  /// be set to zero.
  #[inline(always)]
  pub fn new(ty_id: ExtTyId<'ivm>, payload: Word) -> Self {
    debug_assert!(
      payload.bits() & !Self::PAYLOAD_MASK == 0,
      "ExtVal::new with non-payload bits set"
    );

    unsafe {
      Self::from_bits(payload.map_bits(|payload| {
        ((ty_id.bits() as u64) << 48) | (payload & Self::PAYLOAD_MASK) | (Tag::ExtVal as u64)
      }))
    }
  }

  /// Returns the type id of this value.
  #[inline(always)]
  pub fn ty_id(&self) -> ExtTyId<'ivm> {
    ExtTyId::from_bits((self.0.bits() >> 48) as u16)
  }

  /// Returns the unshifted 45-bit payload of this value.
  ///
  /// The payload is located between bits 48 and 3 in the returned `u64`.
  /// The highest 16 bits and lowest 3 bits are guaranteed to be zero.
  #[inline(always)]
  fn payload(&self) -> Word {
    self.0.map_bits(|bits| bits & Self::PAYLOAD_MASK)
  }

  /// Interprets this extrinsic value as a `T` without checking its [`ExtTyId`].
  ///
  /// # Safety
  ///
  /// This [`ExtVal`]'s payload must be valid to cast into a `T` using
  /// [`ExtTyCast::from_payload`].
  pub unsafe fn cast<T: ExtTyCast<'ivm>>(self) -> T {
    unsafe { T::from_payload(self.payload()) }
  }
}

impl<'ivm> Debug for ExtVal<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ExtVal({:?})", self.0.bits())
  }
}

pub struct ExtTy<'ivm, T>(ExtTyId<'ivm>, PhantomData<fn(T) -> T>);

impl<'ivm, T> Clone for ExtTy<'ivm, T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<'ivm, T> Copy for ExtTy<'ivm, T> {}

impl<'ivm, T> ExtTy<'ivm, T> {
  pub fn new_unchecked(ty_id: ExtTyId<'ivm>) -> Self {
    Self(ty_id, PhantomData)
  }

  #[inline(always)]
  pub fn ty_id(self) -> ExtTyId<'ivm> {
    self.0
  }

  /// Interprets this extrinsic value as a `T` through [`ExtTy<'ivm, T>`].
  ///
  /// If `ext`'s [`ExtVal::ty_id`] does not match [`self::ty_id`], this returns
  /// `None`.
  #[inline(always)]
  pub fn unwrap_ext_val(self, ext: ExtVal<'ivm>) -> Option<T>
  where
    T: ExtTyCast<'ivm>,
  {
    if ext.ty_id() == self.0 { Some(unsafe { T::from_payload(ext.payload()) }) } else { None }
  }

  /// Converts `value` into an [`ExtVal`] with `self` as the type id.
  #[inline(always)]
  pub fn wrap_ext_val(self, value: T) -> ExtVal<'ivm>
  where
    T: ExtTyCast<'ivm>,
  {
    ExtVal::new(self.ty_id(), T::into_payload(value))
  }
}

/// A trait describing how to convert between an [`ExtVal`] and a `Self`.
///
/// Types which can be converted to/from a 45-bit payload, can implement
/// `ExtTyCast`.
pub trait ExtTyCast<'ivm> {
  const COPY: bool;

  /// Converts `Self` into an unshifted [`ExtVal`] 45-bit payload.
  ///
  /// The returned value must not have bits outside of [`ExtVal::PAYLOAD_MASK`].
  /// That is, the highest 16 bits and lowest 3 bits must be set to zero.
  fn into_payload(self) -> Word;

  /// Casts an [`ExtVal`]'s payload to a `Self`.
  ///
  /// # Safety
  ///
  /// `payload` must have been returned from [`into_payload`].
  unsafe fn from_payload(payload: Word) -> Self;
}

impl<'ivm> ExtTyCast<'ivm> for u32 {
  const COPY: bool = true;

  #[inline(always)]
  fn into_payload(self) -> Word {
    Word::from_bits((self as u64) << 3)
  }

  #[inline(always)]
  unsafe fn from_payload(payload: Word) -> u32 {
    (payload.bits() >> 3) as u32
  }
}

impl<'ivm> ExtTyCast<'ivm> for f32 {
  const COPY: bool = true;

  #[inline(always)]
  fn into_payload(self) -> Word {
    Word::from_bits((self.to_bits() as u64) << 3)
  }

  #[inline(always)]
  unsafe fn from_payload(payload: Word) -> f32 {
    f32::from_bits((payload.bits() >> 3) as u32)
  }
}

#[repr(align(8))]
pub struct Aligned<T>(T);

impl<'ivm> ExtTyCast<'ivm> for f64 {
  const COPY: bool = false;

  #[inline(always)]
  fn into_payload(self) -> Word {
    let pointer = Box::into_raw(Box::new(Aligned(self)));
    Word::from_ptr(pointer.cast())
  }

  #[inline(always)]
  unsafe fn from_payload(payload: Word) -> Self {
    let ptr = payload.ptr().cast_mut().cast();
    let Aligned(f) = unsafe { *Box::from_raw(ptr) };
    f
  }
}

#[repr(transparent)]
pub struct ExtIter<T> {
  iter: Box<Aligned<IntoIter<T>>>,
}

impl<T> ExtIter<T> {
  pub fn new(elements: Vec<T>) -> Self {
    Self { iter: Box::new(Aligned(elements.into_iter())) }
  }
}

impl<T> Deref for ExtIter<T> {
  type Target = IntoIter<T>;

  fn deref(&self) -> &Self::Target {
    &self.iter.0
  }
}

impl<T> DerefMut for ExtIter<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.iter.0
  }
}

impl<'ivm, T> ExtTyCast<'ivm> for ExtIter<T> {
  const COPY: bool = false;

  #[inline(always)]
  fn into_payload(self) -> Word {
    let pointer = Box::into_raw(self.iter);
    Word::from_ptr(pointer.cast())
  }

  #[inline(always)]
  unsafe fn from_payload(payload: Word) -> Self {
    let ptr = payload.ptr().cast_mut().cast();
    let iter = unsafe { Box::from_raw(ptr) };
    Self { iter }
  }
}

/// Used for the `IO` extrinsic type.
impl<'ivm> ExtTyCast<'ivm> for () {
  const COPY: bool = false;

  #[inline(always)]
  fn into_payload(self) -> Word {
    Word::from_bits(0)
  }

  #[inline(always)]
  unsafe fn from_payload(_payload: Word) {}
}

/// The type id of an external value.
///
/// The highest bit specifies whether it is copyable. If a non-copyable
/// [`ExtVal`] is copied or erased during reduction, an error flag will
/// be set on the [`IVM`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtTyId<'ivm>(u16, PhantomData<&'ivm mut &'ivm ()>);

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

  #[inline(always)]
  pub fn is_copy(self) -> bool {
    self.0 & Self::COPY_BIT != 0
  }
}

/// A type uniquely identifying an extrinsic function.
///
/// The highest bit denotes whether the function arguments are swapped.
/// The second-highest bit denotes whether this is a merge or split ext fn.
/// The lowest 14 bits denote an index into one of `ext_split_fns` or
/// `ext_merge_fns`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtFn<'ivm>(u16, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtFn<'ivm> {
  const SWAP_BIT: u16 = 0x8000;
  const MERGE_BIT: u16 = 0x4000;
  const ID_MASK: u16 = 0x3FFF;

  #[inline(always)]
  fn new_split(id: usize) -> Self {
    Self(id as u16, PhantomData)
  }

  #[inline(always)]
  fn new_merge(id: usize) -> Self {
    Self(id as u16 | Self::MERGE_BIT, PhantomData)
  }

  #[inline(always)]
  fn index(&self) -> usize {
    (self.0 & Self::ID_MASK) as usize
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
