//! External values and functions, whose semantics live outside interaction
//! combinators.

#![allow(nonstandard_style)]

use core::{
  fmt::{self, Debug},
  marker::PhantomData,
  ops::{Deref, DerefMut},
};

use crate::runtime::{Runtime, port::Tag, wire::Wire, word::Word};

macro_rules! trait_alias {
  ($($(#[$attr:meta])* $vis:vis trait $name:ident = ($($trait:tt)*);)*) => {$(
    $(#[$attr])*
    $vis trait $name<'ivm>: $($trait)* {}
    impl<'ivm, T> $name<'ivm> for T where T: $($trait)* {}
  )*};
}

trait_alias! {
  /// A trait alias for `ExtVal -> (ExtVal, ExtVal)` extrinsic functions.
  pub trait ExtFnSplit = (
    Fn(&mut Runtime<'ivm, '_>, ExtVal<'ivm>, Wire<'ivm>, Wire<'ivm>) + Send + Sync + 'ivm
  );

  /// A trait alias for `(ExtVal, ExtVal) -> ExtVal` extrinsic functions.
  pub trait ExtFnMerge = (
    Fn(&mut Runtime<'ivm, '_>, ExtVal<'ivm>, ExtVal<'ivm>, Wire<'ivm>) + Send + Sync + 'ivm
  );
}

pub struct Extrinsics<'ivm> {
  split: Vec<Box<dyn ExtFnSplit<'ivm>>>,
  merge: Vec<Box<dyn ExtFnMerge<'ivm>>>,

  /// Number of registered extrinsic types.
  tys: u16,

  invariant: PhantomData<&'ivm mut &'ivm ()>,
}

impl<'ivm> Extrinsics<'ivm> {
  /// # Safety
  ///
  /// Only one `Extrinsics` may be constructed for a given `'ivm`.
  pub(crate) unsafe fn new() -> Self {
    Extrinsics { split: Vec::new(), merge: Vec::new(), tys: 0, invariant: PhantomData }
  }

  pub const MAX_EXT_FN_COUNT: usize = 0x3FFF;
  pub const MAX_EXT_TY_COUNT: usize = 0x7FFF;

  pub fn new_ext_fn_split(&mut self, f: impl ExtFnSplit<'ivm>) -> ExtFnId<'ivm> {
    assert!(self.split.len() < Self::MAX_EXT_FN_COUNT);

    let ext_fn = ExtFnId::new_split(self.split.len());
    self.split.push(Box::new(f));
    ext_fn
  }

  pub fn new_ext_fn_merge(&mut self, f: impl ExtFnMerge<'ivm>) -> ExtFnId<'ivm> {
    assert!(self.merge.len() < Self::MAX_EXT_FN_COUNT);

    let ext_fn = ExtFnId::new_merge(self.merge.len());
    self.merge.push(Box::new(f));
    ext_fn
  }

  pub fn new_ext_ty<T: ExtTyCast<'ivm>>(&mut self) -> ExtTy<'ivm, T> {
    assert!((self.tys as usize) < Self::MAX_EXT_TY_COUNT);

    self.tys += 1;
    unsafe { ExtTy::new_unchecked(ExtTyId::new(self.tys, T::COPY)) }
  }

  pub fn get_ext_fn_split(&self, ext_fn: ExtFnId<'ivm>) -> &dyn ExtFnSplit<'ivm> {
    unsafe { self.split.get_unchecked(ext_fn.index()) }
  }

  pub fn get_ext_fn_merge(&self, ext_fn: ExtFnId<'ivm>) -> &dyn ExtFnMerge<'ivm> {
    unsafe { self.merge.get_unchecked(ext_fn.index()) }
  }
}

/// An external value with a 45-bit payload.
///
/// The 64-bit value is laid out as follows:
/// - top 16 bits are the extrinsic's type id [`ExtTyId`].
/// - the next 45 bits are the payload.
/// - the last 3 bits are `Tag::ExtVal as u16` for compatibility with [`Port`].
///
/// [`Port`]: crate::vm::Port
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
  ///
  /// ## Safety
  /// The `payload` must be valid for the given `ty_id`.
  #[inline(always)]
  pub unsafe fn new(ty_id: ExtTyId<'ivm>, payload: Word) -> Self {
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
}

impl<'ivm> Debug for ExtVal<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ExtVal({:?}, {:?})", self.ty_id(), self.payload().ptr())
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
  /// ## Safety
  /// `ty_id` must actually correspond to the type `T`.
  pub unsafe fn new_unchecked(ty_id: ExtTyId<'ivm>) -> Self {
    Self(ty_id, PhantomData)
  }

  #[inline(always)]
  pub fn id(self) -> ExtTyId<'ivm> {
    self.0
  }

  /// Interprets this extrinsic value as a `T` through [`ExtTy<'ivm, T>`].
  ///
  /// If `ext`'s [`ExtVal::ty_id`] does not match [`self::ty_id`], this returns
  /// `None`.
  #[inline(always)]
  pub fn unwrap(self, rt: &mut Runtime<'ivm, '_>, ext: ExtVal<'ivm>) -> Option<T>
  where
    T: ExtTyCast<'ivm>,
  {
    (ext.ty_id() == self.0).then(|| unsafe { T::from_payload(rt, ext.payload()) })
  }

  /// Converts `value` into an [`ExtVal`] with `self` as the type id.
  #[inline(always)]
  pub fn wrap(self, rt: &mut Runtime<'ivm, '_>, value: T) -> ExtVal<'ivm>
  where
    T: ExtTyCast<'ivm>,
  {
    unsafe { ExtVal::new(self.id(), T::into_payload(rt, value)) }
  }

  /// Interprets this extrinsic value as a `T` through [`ExtTy<'ivm, T>`].
  ///
  /// If `ext`'s [`ExtVal::ty_id`] does not match [`self::ty_id`], this returns
  /// `None`.
  #[inline(always)]
  pub fn unwrap_static(self, ext: ExtVal<'ivm>) -> Option<T>
  where
    T: ExtTyCastStatic<'ivm>,
  {
    (ext.ty_id() == self.0).then(|| unsafe { T::from_payload_static(ext.payload()) })
  }

  /// Converts `value` into an [`ExtVal`] with `self` as the type id statically.
  #[inline(always)]
  pub fn wrap_static(self, value: T) -> ExtVal<'ivm>
  where
    T: ExtTyCastStatic<'ivm>,
  {
    unsafe { ExtVal::new(self.id(), T::into_payload_static(value)) }
  }
}

/// Conversion between an [`ExtVal`] and a `Self` at runtime.
pub trait ExtTyCast<'ivm> {
  const COPY: bool;

  /// Converts `Self` into an unshifted [`ExtVal`] 45-bit payload.
  ///
  /// The returned value must not have bits outside of [`ExtVal::PAYLOAD_MASK`].
  /// That is, the highest 16 bits and lowest 3 bits must be set to zero.
  fn into_payload(rt: &mut Runtime<'ivm, '_>, value: Self) -> Word;

  /// Casts an [`ExtVal`]'s payload to a `Self`.
  ///
  /// # Safety
  ///
  /// `payload` must have been returned from [`into_payload`].
  unsafe fn from_payload(rt: &mut Runtime<'ivm, '_>, payload: Word) -> Self;
}

/// Conversion between an [`ExtVal`] and a `Self` statically.
pub trait ExtTyCastStatic<'ivm> {
  const COPY: bool;

  /// Converts `Self` into an unshifted [`ExtVal`] 45-bit payload.
  ///
  /// The returned value must not have bits outside of [`ExtVal::PAYLOAD_MASK`].
  /// That is, the highest 16 bits and lowest 3 bits must be set to zero.
  fn into_payload_static(value: Self) -> Word;

  /// Casts an [`ExtVal`]'s payload to a `Self`.
  ///
  /// # Safety
  ///
  /// `payload` must have been returned from [`into_payload`].
  unsafe fn from_payload_static(payload: Word) -> Self;
}

impl<'ivm, T: ExtTyCastStatic<'ivm>> ExtTyCast<'ivm> for T {
  const COPY: bool = T::COPY;

  fn into_payload(_: &mut Runtime<'ivm, '_>, value: Self) -> Word {
    T::into_payload_static(value)
  }

  unsafe fn from_payload(_: &mut Runtime<'ivm, '_>, payload: Word) -> Self {
    unsafe { T::from_payload_static(payload) }
  }
}

#[derive(Default)]
#[repr(align(8))]
pub struct Aligned<T>(T);

#[derive(Default)]
#[repr(transparent)]
pub struct Boxed<T>(Box<Aligned<T>>);

impl<T> From<T> for Boxed<T> {
  fn from(value: T) -> Self {
    Self(Box::new(Aligned(value)))
  }
}

impl<T> Deref for Boxed<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.0.0
  }
}

impl<T> DerefMut for Boxed<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0.0
  }
}

impl<T> Boxed<T> {
  pub fn new(value: T) -> Self {
    Boxed(Box::new(Aligned(value)))
  }

  fn ptr(self) -> *mut Aligned<T> {
    Box::into_raw(self.0)
  }

  pub fn into_inner(self) -> T {
    (*self.0).0
  }

  unsafe fn from_aligned_ptr(ptr: *mut Aligned<T>) -> Self {
    Self(unsafe { Box::from_raw(ptr) })
  }
}

impl<'ivm, T> ExtTyCastStatic<'ivm> for Boxed<T> {
  const COPY: bool = false;

  #[inline(always)]
  fn into_payload_static(value: Boxed<T>) -> Word {
    Word::from_ptr(value.ptr().cast())
  }

  #[inline(always)]
  unsafe fn from_payload_static(payload: Word) -> Self {
    unsafe { Boxed::from_aligned_ptr(payload.ptr().cast_mut().cast()) }
  }
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
pub struct ExtFnId<'ivm>(u16, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtFnId<'ivm> {
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

impl<'ivm> Debug for ExtFnId<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ExtFn({:?})", self.0)
  }
}
