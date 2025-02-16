//! External values and functions, whose semantics live outside interaction
//! combinators.

#![allow(nonstandard_style)]

use core::{
  fmt::{self, Debug},
  marker::PhantomData,
};

use crate::port::Tag;

#[derive(Default)]
pub struct Extrinsics<'ivm> {
  ext_fns: Vec<Box<dyn Fn(ExtVal<'ivm>, ExtVal<'ivm>) -> ExtVal<'ivm> + Sync + 'ivm>>,

  /// Number of registered light (unboxed) ext types
  light_ext_ty: u16,

  n32_ext_ty: Option<ExtTy<'ivm>>,

  phantom: PhantomData<fn(&'ivm ()) -> &'ivm ()>,
}

impl<'ivm> Extrinsics<'ivm> {
  pub const MAX_EXT_FN_KIND_COUNT: usize = 0x7FFF;
  pub const MAX_LIGHT_EXT_TY_COUNT: usize = 0x7FFF;

  pub fn register_ext_fn(
    &mut self,
    f: impl Fn([ExtVal<'ivm>; 2]) -> [ExtVal<'ivm>; 1] + Sync + 'ivm,
  ) -> ExtFn<'ivm> {
    if self.ext_fns.len() >= Self::MAX_EXT_FN_KIND_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic functions.");
    } else {
      let ext_fn = ExtFn(self.ext_fns.len() as u16, PhantomData);
      self.ext_fns.push(Box::new(move |a, b| f([a, b])[0]));
      ext_fn
    }
  }

  pub fn register_light_ext_ty(&mut self) -> ExtTy<'ivm> {
    if self.light_ext_ty as usize >= Self::MAX_LIGHT_EXT_TY_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic unboxed types.");
    } else {
      let ext_ty = ExtTy::from_id_and_rc(self.light_ext_ty, false);
      self.light_ext_ty += 1;
      ext_ty
    }
  }

  pub fn register_n32_ext_ty(&mut self) -> ExtTy<'ivm> {
    let n32_ext_ty = self.register_light_ext_ty();
    assert!(self.n32_ext_ty.replace(n32_ext_ty).is_none());
    n32_ext_ty
  }

  pub fn call(
    &self,
    ext_fn: ExtFn<'ivm>,
    mut arg0: ExtVal<'ivm>,
    mut arg1: ExtVal<'ivm>,
  ) -> ExtVal<'ivm> {
    let closure =
      self.ext_fns.get(ext_fn.kind() as usize).expect("ext_fn with given ID not found!");
    if ext_fn.is_swapped() {
      (arg0, arg1) = (arg1, arg0)
    };
    (closure)(arg0, arg1)
  }

  pub fn ext_val_as_n32(&self, val: ExtVal<'ivm>) -> u32 {
    assert!(self.n32_ext_ty.is_some_and(move |x| val.ty() == x));
    val.payload()
  }
}

/// An external value.
///
/// The top 32 bits are the *payload*, and the 16 bits after that the *type* (an
/// [`ExtTy`]). The interpretation of the payload depends on the type.
///
/// The bottom 16 bits are always `Tag::ExtVal as u16` for bit-compatibility
/// with `ExtVal` ports.
#[derive(Clone, Copy)]
pub struct ExtVal<'ivm>(u64, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtVal<'ivm> {
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
  pub const fn new(ty: ExtTy<'ivm>, payload: u32) -> Self {
    Self(payload as (u64) << 32 | ty.id() as (u64) << 16 | Tag::ExtVal as u64, PhantomData)
  }
  /// Accesses the type of this value.
  #[inline(always)]
  pub fn ty(&self) -> ExtTy<'ivm> {
    ExtTy((self.0 >> 16) as u16, self.1)
  }

  /// Accesses the payload of this value.
  #[inline(always)]
  pub fn payload(&self) -> u32 {
    (self.0 >> 32) as u32
  }

  #[inline(always)]
  pub fn as_ty(&self, &ty: &ExtTy<'ivm>) -> u32 {
    assert!(self.ty() == ty);
    (self.0 >> 32) as u32
  }
}

impl<'ivm> Debug for ExtVal<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ExtVal({:?})", self.0)
  }
}

/// The type of an external value.
/// The higher bit specifies whether it is reference-counted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtTy<'ivm>(u16, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtTy<'ivm> {
  fn from_id_and_rc(n: u16, rc: bool) -> Self {
    Self(n | if rc { 0x8000 } else { 0 }, PhantomData)
  }

  const fn id(self) -> u16 {
    self.0
  }
}

/// A reference to an external function. The lower 15 bits are an [`ExtFnKind`],
/// determining the logic of the function, and the top bit denotes whether the
/// parameters to the function have been swapped.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtFn<'ivm>(u16, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtFn<'ivm> {
  const SWAP_BIT: u16 = 0x8000;

  #[inline(always)]
  pub fn bits(&self) -> u16 {
    self.0
  }

  #[inline(always)]
  pub fn kind(&self) -> u16 {
    self.0 & !Self::SWAP_BIT
  }

  #[inline(always)]
  pub fn is_swapped(&self) -> bool {
    (self.0 >> 15) == 1
  }

  #[inline(always)]
  pub fn swap(&self) -> Self {
    Self(self.0 ^ Self::SWAP_BIT, self.1)
  }
}

impl<'ivm> Debug for ExtFn<'ivm> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ExtFn({:?})", self.0)
  }
}
