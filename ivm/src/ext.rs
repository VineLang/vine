//! External values and functions, whose semantics live outside interaction
//! combinators.

#![allow(nonstandard_style)]

use core::{
  fmt::{self, Debug},
  mem::transmute,
  ops::{Add, Div, Mul, Sub},
};
use std::{
  collections::HashMap,
  io::{self, Read, Write},
  marker::PhantomData,
};

use crate::port::Tag;

macro_rules! define_ext_fns {
  ($map:ident,$self:ident, $($name:expr => |$a:ident, $b: ident| $body:expr ),*) => {
    $($map.insert($name.to_string(), $self.register_ext_fn(move |[$a, $b]| [$body]).unwrap());)*
  };
}
#[derive(Default)]
pub struct Extrinsics<'ivm> {
  ext_fns: Vec<Box<dyn Fn([ExtVal<'ivm>; 2]) -> [ExtVal<'ivm>; 1] + Sync + 'ivm>>,
  // amount of registered light exttys
  light_ext_ty: u16,
  // This is a list of drop-functions.
  rc_ext_ty: Vec<fn(u32)>,

  n32_ext_ty: Option<ExtTy<'ivm>>,
  io_ext_ty: Option<ExtTy<'ivm>>,

  phantom: PhantomData<fn(&'ivm ()) -> &'ivm ()>,
}

impl<'ivm> Extrinsics<'ivm> {
  pub const MAX_EXT_FN_KIND_COUNT: usize = 0x8FFF;
  pub const MAX_LIGHT_EXT_TY_COUNT: usize = 0x8FFF;
  pub const MAX_RC_EX_TTY_COUNT: usize = 0x8FFF;

  pub fn register_ext_fn(
    &mut self,
    f: impl Fn([ExtVal<'ivm>; 2]) -> [ExtVal<'ivm>; 1] + Sync + 'ivm,
  ) -> Option<ExtFn<'ivm>> {
    if self.ext_fns.len() >= Self::MAX_EXT_FN_KIND_COUNT {
      return None;
    } else {
      let ext_fn = ExtFn(self.ext_fns.len() as u16, PhantomData);
      self.ext_fns.push(Box::new(f));
      return Some(ext_fn);
    }
  }
  pub fn register_light_ext_ty(&mut self) -> Option<ExtTy<'ivm>> {
    if self.light_ext_ty as usize >= Self::MAX_LIGHT_EXT_TY_COUNT {
      return None;
    } else {
      let ext_ty = ExtTy::from_id_and_rc(self.light_ext_ty, false);
      self.light_ext_ty += 1;
      return Some(ext_ty);
    }
  }
  /// Some extrinsics are built-in with IVM
  pub fn register_builtin_extrinsics(
    &mut self,
  ) -> (HashMap<String, ExtFn<'ivm>>, HashMap<String, ExtTy<'ivm>>) {
    let mut ext_fn_map = HashMap::new();
    let mut ext_ty_map = HashMap::new();
    let n32_ext_ty = self.register_light_ext_ty().expect("reached maximum amount of extrinsics!");
    let f32_ext_ty = self.register_light_ext_ty().expect("reached maximum amount of extrinsics!");
    let io_ext_ty = self.register_light_ext_ty().expect("reached maximum amount of extrinsics!");
    ext_ty_map.insert("N32".into(), n32_ext_ty);
    ext_ty_map.insert("F32".into(), f32_ext_ty);
    ext_ty_map.insert("IO".into(), io_ext_ty);
    self.n32_ext_ty = Some(n32_ext_ty);
    self.io_ext_ty = Some(io_ext_ty);
    enum NumericType {
      F32,
      N32,
    }
    fn ty_to_numeric_type<'ivm>(
      a: ExtVal<'ivm>,
      n32_ext_ty: ExtTy<'ivm>,
      f32_ext_ty: ExtTy<'ivm>,
    ) -> Option<NumericType> {
      if a.ty() == n32_ext_ty {
        Some(NumericType::N32)
      } else if a.ty() == f32_ext_ty {
        Some(NumericType::F32)
      } else {
        None
      }
    }
    fn numeric_op<'ivm>(
      n32_ext_ty: ExtTy<'ivm>,
      f32_ext_ty: ExtTy<'ivm>,
      a: ExtVal<'ivm>,
      b: ExtVal<'ivm>,
      f_n32: fn(u32, u32) -> u32,
      f_f32: fn(f32, f32) -> f32,
    ) -> ExtVal<'ivm> {
      match (
        ty_to_numeric_type(a, n32_ext_ty, f32_ext_ty),
        ty_to_numeric_type(b, n32_ext_ty, f32_ext_ty),
      ) {
        (Some(NumericType::N32), Some(NumericType::N32)) => {
          ExtVal::new(n32_ext_ty, f_n32(a.as_ty(&n32_ext_ty), b.as_ty(&n32_ext_ty)))
        }
        (Some(NumericType::F32), Some(NumericType::F32)) => ExtVal::new(
          f32_ext_ty,
          f_f32(f32::from_bits(a.as_ty(&f32_ext_ty)), f32::from_bits(b.as_ty(&f32_ext_ty)))
            .to_bits(),
        ),
        (Some(NumericType::N32), Some(NumericType::F32)) => ExtVal::new(
          f32_ext_ty,
          f_f32(a.as_ty(&n32_ext_ty) as f32, f32::from_bits(b.as_ty(&f32_ext_ty))).to_bits(),
        ),
        (Some(NumericType::F32), Some(NumericType::N32)) => ExtVal::new(
          f32_ext_ty,
          f_f32(f32::from_bits(a.as_ty(&f32_ext_ty)), b.as_ty(&n32_ext_ty) as f32).to_bits(),
        ),
        _ => unimplemented!(),
      }
    }

    fn comparison<'ivm>(
      n32_ext_ty: ExtTy<'ivm>,
      f32_ext_ty: ExtTy<'ivm>,
      a: ExtVal<'ivm>,
      b: ExtVal<'ivm>,
      f_u32: fn(u32, u32) -> bool,
      f_f32: fn(f32, f32) -> bool,
    ) -> ExtVal<'ivm> {
      ExtVal::new(
        n32_ext_ty,
        u32::from(
          match (
            ty_to_numeric_type(a, n32_ext_ty, f32_ext_ty),
            ty_to_numeric_type(b, n32_ext_ty, f32_ext_ty),
          ) {
            (Some(NumericType::N32), Some(NumericType::N32)) => {
              f_u32(a.as_ty(&n32_ext_ty), b.as_ty(&n32_ext_ty))
            }
            (Some(NumericType::F32), Some(NumericType::F32)) => {
              f_f32(f32::from_bits(a.as_ty(&f32_ext_ty)), f32::from_bits(b.as_ty(&f32_ext_ty)))
            }
            (Some(NumericType::N32), Some(NumericType::F32)) => {
              f_f32(a.as_ty(&n32_ext_ty) as f32, f32::from_bits(b.as_ty(&f32_ext_ty)))
            }
            (Some(NumericType::F32), Some(NumericType::N32)) => {
              f_f32(f32::from_bits(a.as_ty(&f32_ext_ty)), b.as_ty(&n32_ext_ty) as f32)
            }
            _ => unimplemented!(),
          },
        ),
      )
    }
    define_ext_fns!(ext_fn_map, self,
      "seq" => |a, b| a,

      "add" => |a, b| numeric_op(n32_ext_ty, f32_ext_ty, a, b, u32::wrapping_add, f32::add),
      "sub" => |a, b| numeric_op(n32_ext_ty, f32_ext_ty, a, b, u32::wrapping_sub, f32::sub),
      "mul" => |a, b| numeric_op(n32_ext_ty, f32_ext_ty, a, b, u32::wrapping_mul, f32::mul),
      "div" => |a, b| numeric_op(n32_ext_ty, f32_ext_ty, a, b, u32::wrapping_div, f32::div),
      "rem" => |a, b| numeric_op(n32_ext_ty, f32_ext_ty, a, b, u32::wrapping_rem, f32::rem_euclid),

      "eq" => |a, b| comparison(n32_ext_ty, f32_ext_ty, a, b, |a, b| a == b, |a, b| a == b),
      "ne" => |a, b| comparison(n32_ext_ty, f32_ext_ty, a, b, |a, b| a != b, |a, b| a != b),
      "lt" => |a, b| comparison(n32_ext_ty, f32_ext_ty, a, b, |a, b| a < b, |a, b| a < b),
      "le" => |a, b| comparison(n32_ext_ty, f32_ext_ty, a, b, |a, b| a <= b, |a, b| a <= b),

      "n32_shl" => |a, b| ExtVal::new(n32_ext_ty, a.as_ty(&n32_ext_ty).wrapping_shl(b.as_ty(&n32_ext_ty))),
      "n32_shr" => |a, b| ExtVal::new(n32_ext_ty, a.as_ty(&n32_ext_ty).wrapping_shr(b.as_ty(&n32_ext_ty))),
      "n32_rotl" => |a, b| ExtVal::new(n32_ext_ty, a.as_ty(&n32_ext_ty).rotate_left(b.as_ty(&n32_ext_ty))),
      "n32_rotr" => |a, b| ExtVal::new(n32_ext_ty, a.as_ty(&n32_ext_ty).rotate_right(b.as_ty(&n32_ext_ty))),

      "n32_and" => |a, b| ExtVal::new(n32_ext_ty, a.as_ty(&n32_ext_ty) & b.as_ty(&n32_ext_ty)),
      "n32_or" => |a, b| ExtVal::new(n32_ext_ty, a.as_ty(&n32_ext_ty) | b.as_ty(&n32_ext_ty)),
      "n32_xor" => |a, b| ExtVal::new(n32_ext_ty, a.as_ty(&n32_ext_ty) ^ b.as_ty(&n32_ext_ty)),

      "n32_add_high" => |a, b| ExtVal::new(n32_ext_ty, (((a.as_ty(&n32_ext_ty) as u64) + (b.as_ty(&n32_ext_ty) as u64)) >> 32) as u32),
      "n32_mul_high" => |a, b| ExtVal::new(n32_ext_ty, (((a.as_ty(&n32_ext_ty) as u64) * (b.as_ty(&n32_ext_ty) as u64)) >> 32) as u32),

      "io_print_char" => |a, b| {
        a.as_ty(&io_ext_ty);
        print!("{}", char::try_from(b.as_ty(&n32_ext_ty)).unwrap());
        ExtVal::new(io_ext_ty, 0)
      },
      "io_print_byte" => |a, b| {
        a.as_ty(&io_ext_ty);
        io::stdout().write_all(&[b.as_ty(&n32_ext_ty) as u8]).unwrap();
        ExtVal::new(io_ext_ty, 0)
      },
      "io_flush" => |a, b| {
        a.as_ty(&io_ext_ty);
        io::stdout().flush().unwrap();
        ExtVal::new(io_ext_ty, 0)
      },
      "io_read_byte" => |a, b| {
        a.as_ty(&io_ext_ty);
        let default = b.as_ty(&n32_ext_ty) as u8;
        let mut buf = [default];
        _ = io::stdin().read(&mut buf).unwrap();
        ExtVal::new(n32_ext_ty, buf[0] as u32)
      }
    );
    (ext_fn_map, ext_ty_map)
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
    (closure)([arg0, arg1])[0]
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
  fn from_id(n: u16) -> Self {
    Self(n, PhantomData)
  }
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
