//! External values and functions, whose semantics live outside interaction
//! combinators.

#![allow(nonstandard_style)]

use crate::arc::{Arc, ArcInner};
use core::{
  fmt::{self, Debug},
  marker::PhantomData,
};
use std::{
  any::{Any, TypeId},
  collections::BTreeMap,
};

use crate::{port::Tag, word::Word};

#[derive(Default)]
pub struct Extrinsics<'ivm> {
  ext_fns: Vec<Box<dyn Fn(ExtVal<'ivm>, ExtVal<'ivm>) -> ExtVal<'ivm> + Sync + 'ivm>>,

  unboxed_ext_ty: u16,

  rc_ext_ty: Vec<unsafe fn(*mut ())>,

  auto_rc_ty_ids: BTreeMap<TypeId, u16>,

  n32_ext_ty: Option<ExtTy<'ivm>>,

  phantom: PhantomData<fn(&'ivm ()) -> &'ivm ()>,
}

impl<'ivm> Extrinsics<'ivm> {
  pub const MAX_EXT_FN_KIND_COUNT: usize = 0x7FFF;
  pub const MAX_UNBOXED_EXT_TY_COUNT: usize = 0x7FFF;
  pub const MAX_RC_EXT_TY_COUNT: usize = 0x7FFF;

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

  pub fn register_unboxed_ext_ty(&mut self) -> ExtTy<'ivm> {
    if self.unboxed_ext_ty as usize >= Self::MAX_UNBOXED_EXT_TY_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic unboxed types.");
    } else {
      let ext_ty = ExtTy::from_id_and_rc(self.unboxed_ext_ty, false);
      self.unboxed_ext_ty += 1;
      ext_ty
    }
  }
  pub fn register_rc_ext_ty(&mut self, drop_in_place_function: unsafe fn(*mut ())) -> ExtTy<'ivm> {
    if self.rc_ext_ty.len() as usize >= Self::MAX_RC_EXT_TY_COUNT {
      panic!("IVM reached maximum amount of registered extrinsic reference-counted types.");
    } else {
      let ext_ty = ExtTy::from_id_and_rc(self.rc_ext_ty.len() as u16, true);
      self.rc_ext_ty.push(drop_in_place_function);
      ext_ty
    }
  }
  pub unsafe fn set_rc_ext_ty_type_id(&mut self, ty: ExtTy<'ivm>, type_id: TypeId) {
    assert!(ty.is_rc());
    assert!(self.auto_rc_ty_ids.insert(type_id, ty.id()).is_none());
  }
  pub fn register_n32_ext_ty(&mut self) -> ExtTy<'ivm> {
    let n32_ext_ty = self.register_unboxed_ext_ty();
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

  pub fn erase_ext_val(&self, val: ExtVal<'ivm>) {
    if val.ty().is_rc() {
      let function = self.rc_ext_ty.get(val.ty().id() as usize).unwrap();
      let data = val.pointer_payload();
      let arc = Arc::from_raw(data as *const ArcInner<()>);
      unsafe {
        Arc::drop_with(arc, *function);
      }
    }
  }

  pub fn dup_ext_val(&self, val: ExtVal<'ivm>) -> (ExtVal<'ivm>, ExtVal<'ivm>) {
    if val.ty().is_rc() {
      let ty = val.ty();
      let data = val.pointer_payload();
      let arc = Arc::from_raw(data as *const ArcInner<()>);
      let arc2 = arc.clone();
      unsafe {
        (
          ExtVal::new_rc_raw(ty, Arc::into_raw(arc) as _),
          ExtVal::new_rc_raw(ty, Arc::into_raw(arc2) as _),
        )
      }
    } else {
      (val, val)
    }
  }

  pub fn new_rc_instance<T: Any>(&self, data: T) -> ExtVal<'ivm> {
    let ty_id = self.auto_rc_ty_ids.get(&data.type_id()).expect(
      "Attempted to create instance of reference-counted data type that has not been registered!",
    );
    unsafe {
      ExtVal::new_rc_raw(ExtTy::from_id_and_rc(*ty_id, true), Arc::new(data).into_raw() as _)
    }
  }

  pub fn ext_val_as_n32(&self, val: ExtVal<'ivm>) -> u32 {
    assert!(self.n32_ext_ty.is_some_and(move |x| val.ty() == x));
    val.numeric_payload()
  }
}

/// An external value.
///
/// The top 16 bits are the `[`ExtTy`], the next 45 bits are the [`Payload`],
/// and the final 3 bits are the tag.
#[derive(Clone, Copy)]
pub struct ExtVal<'ivm>(Word, PhantomData<fn(&'ivm ()) -> &'ivm ()>);

impl<'ivm> ExtVal<'ivm> {
  /// Creates an `ExtVal` from the raw bits.
  ///
  /// ## Safety
  /// The bits must be a valid representation of an `ExtVal`.
  #[inline(always)]
  pub(crate) const unsafe fn from_word(word: Word) -> Self {
    Self(word, PhantomData)
  }

  #[inline(always)]
  pub const fn new_unboxed(ty: ExtTy<'ivm>, payload: u32) -> Self {
    assert!(!ty.is_rc());
    unsafe {
      Self::from_word(Word::from_bits(
        ty.0 as (u64) << 48 | payload as (u64) << 3 | Tag::ExtVal as (u64),
      ))
    }
  }

  #[inline(always)]
  pub unsafe fn new_rc_raw(ty: ExtTy<'ivm>, pointer: *const ()) -> Self {
    assert!(ty.is_rc());
    unsafe {
      Self::from_word(Word::from_ptr(pointer).map_bits(|_| {
        let payload = pointer.addr() as u64;
        // Ensure the payload pointer does not conflict with any other field
        assert!(payload & 0xFF000007 == 0);
        ty.0 as (u64) << 48 | payload as u64 | Tag::ExtVal as u64
      }))
    }
  }
  /// Accesses the type of this value.
  #[inline(always)]
  pub fn ty(&self) -> ExtTy<'ivm> {
    ExtTy((self.0.bits() >> 48) as u16, self.1)
  }

  #[inline(always)]
  pub(crate) fn word(&self) -> Word {
    self.0
  }

  /// Accesses the payload of this value.
  #[inline(always)]
  pub fn numeric_payload(&self) -> u32 {
    (self.0.bits() >> 3) as u32
  }
  #[inline(always)]
  pub fn pointer_payload(&self) -> *const () {
    self.0.map_bits(|x| x & !0xFF000007).ptr()
  }
  #[inline(always)]
  pub fn as_unboxed_ty(&self, &ty: &ExtTy<'ivm>) -> u32 {
    assert!(self.ty() == ty);
    assert!(!ty.is_rc());
    self.numeric_payload()
  }
  #[inline(always)]
  pub fn as_rc_ty(&self, &ty: &ExtTy<'ivm>) -> *const () {
    assert!(self.ty() == ty);
    assert!(!ty.is_rc());
    self.pointer_payload()
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
    self.0 & 0x7FFF
  }
  const fn is_rc(self) -> bool {
    self.0 >> 15 != 0
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
