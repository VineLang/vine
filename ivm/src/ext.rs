//! External values and functions, whose semantics live outside interaction
//! combinators.

#![allow(nonstandard_style)]

use core::{
  fmt::{self, Debug},
  marker::PhantomData,
};

use crate::{ivm::IVM, port::Tag, wire::Wire};

type ExtFnClosure<'ivm> = Box<
  dyn for<'a, 'ext> Fn(*const (), &'a mut IVM<'ivm, 'ext>, ExtVal<'ivm>, ExtVal<'ivm>, Wire<'ivm>),
>;

pub struct Extrinsics<'ivm> {
  ext_fns: Vec<(*const (), ExtFnClosure<'ivm>)>,

  /// Number of registered extrinsic types.
  ext_tys: u16,

  /// An always-present extrinsic type of n32 numbers.
  n32_ext_ty: ExtTy<'ivm>,

  phantom: PhantomData<fn(&'ivm ()) -> &'ivm ()>,
}

unsafe impl Send for Extrinsics<'_> {}
unsafe impl Sync for Extrinsics<'_> {}

impl Default for Extrinsics<'_> {
  fn default() -> Self {
    let n32_ext_ty = ExtTy::new(0, false);

    Self { ext_fns: Vec::new(), ext_tys: 1, n32_ext_ty, phantom: PhantomData }
  }
}

impl<'ivm> Extrinsics<'ivm> {
  pub const MAX_EXT_FN_COUNT: usize = 0x7FFF;
  pub const MAX_EXT_TY_COUNT: usize = 0x7FFF;

  pub fn new_ext_fn(
    &mut self,
    f: impl for<'a, 'ext> Fn(&'ivm (), &'a mut IVM<'ivm, 'ext>, ExtVal<'ivm>, ExtVal<'ivm>, Wire<'ivm>),
  ) -> ExtFn<'ivm> {
    self.new_ext_fn_with_context(f, ())
  }

  pub fn new_ext_fn_with_context<C: 'ivm>(
    &mut self,
    f: impl for<'a, 'ext> Fn(&'ivm C, &'a mut IVM<'ivm, 'ext>, ExtVal<'ivm>, ExtVal<'ivm>, Wire<'ivm>),
    ctx: C,
  ) -> ExtFn<'ivm> {
    if self.ext_fns.len() >= Self::MAX_EXT_FN_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic functions.");
    } else {
      let ext_fn = ExtFn(self.ext_fns.len() as u16, PhantomData);
      let ctx = Box::into_raw(Box::new(ctx));
      let f = Box::new(f)
        as Box<
          dyn for<'a, 'ext> Fn(
            &'ivm C,
            &'a mut IVM<'ivm, 'ext>,
            ExtVal<'ivm>,
            ExtVal<'ivm>,
            Wire<'ivm>,
          ),
        >;
      let f = unsafe { std::mem::transmute(f) };
      self.ext_fns.push((ctx.cast(), f));
      ext_fn
    }
  }

  pub fn new_ext_ty(&mut self, linear: bool) -> ExtTy<'ivm> {
    if self.ext_tys as usize >= Self::MAX_EXT_TY_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic unboxed types.");
    } else {
      let ext_ty = ExtTy::new(self.ext_tys, linear);
      self.ext_tys += 1;
      ext_ty
    }
  }

  pub fn n32_ext_ty(&self) -> ExtTy<'ivm> {
    self.n32_ext_ty
  }

  pub fn ext_val_as_n32(&self, val: ExtVal<'ivm>) -> u32 {
    assert!(self.n32_ext_ty == val.ty());
    val.payload()
  }

  pub fn get_ext_fn(&self, ext_fn: ExtFn<'ivm>) -> &(*const (), ExtFnClosure<'ivm>) {
    let ext_fn_idx = ext_fn.kind() as usize;

    self.ext_fns.get(ext_fn_idx).expect("unknown extrinsic function")
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
///
/// The highest bit specifies whether it is linear.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtTy<'ivm>(u16, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtTy<'ivm> {
  const LINEAR_BIT: u16 = 0x8000;

  fn new(ty_id: u16, linear: bool) -> Self {
    Self(ty_id | if linear { Self::LINEAR_BIT } else { 0 }, PhantomData)
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

#[derive(Debug, Clone, Copy)]
pub struct OpaqueExtFn {
  pub label: u16,
  pub swap: bool,
}
