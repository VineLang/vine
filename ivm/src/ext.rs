//! External values and functions, whose semantics live outside interaction
//! combinators.

#![allow(nonstandard_style)]

use core::{
  fmt::{self, Debug},
  mem::transmute,
  ops::{Add, Div, Mul, Sub},
};
use std::io::{self, Read, Write};

use crate::port::Tag;

/// An external value.
///
/// The top 32 bits are the *payload*, and the 16 bits after that the *type* (an
/// [`ExtTy`]). The interpretation of the payload depends on the type.
///
/// The bottom 16 bits are always `Tag::ExtVal as u16` for bit-compatibility
/// with `ExtVal` ports.
#[derive(Clone, Copy)]
pub struct ExtVal(u64);

impl ExtVal {
  pub const IO: Self = Self::new(ExtTy::IO, 0);

  /// Creates an `ExtVal` from the raw bits.
  ///
  /// ## Safety
  /// The bits must be a valid representation of an `ExtVal`.
  #[inline(always)]
  pub unsafe fn from_bits(bits: u64) -> Self {
    Self(bits)
  }

  /// Returns the raw bits of this value.
  #[inline(always)]
  pub fn bits(&self) -> u64 {
    self.0
  }

  /// Creates a new `ExtVal` with a given type and payload.
  #[inline(always)]
  const fn new(ty: ExtTy, payload: u32) -> Self {
    Self(payload as (u64) << 32 | ty as (u64) << 16 | Tag::ExtVal as u64)
  }

  /// Creates a new `u32` with the given numeric value.
  #[inline(always)]
  pub fn new_u32(value: u32) -> Self {
    Self::new(ExtTy::u32, value)
  }

  /// Creates a new `f32` with the given numeric value.
  #[inline(always)]
  pub fn new_f32(value: f32) -> Self {
    Self::new(ExtTy::f32, value.to_bits())
  }

  /// Accesses the type of this value.
  #[inline(always)]
  pub fn ty(&self) -> ExtTy {
    unsafe { transmute::<u16, ExtTy>((self.0 >> 16) as u16) }
  }

  /// Accesses the payload of this value.
  #[inline(always)]
  pub fn payload(&self) -> u32 {
    (self.0 >> 32) as u32
  }

  /// Asserts that the type of this value is `u32`, and returns the numeric
  /// value.
  #[inline(always)]
  pub fn as_u32(self) -> u32 {
    assert_eq!(self.ty(), ExtTy::u32);
    self.payload()
  }

  /// Asserts that the type of this value is `f32`, and returns the numeric
  /// value.
  #[inline(always)]
  pub fn as_f32(self) -> f32 {
    assert_eq!(self.ty(), ExtTy::f32);
    f32::from_bits(self.payload())
  }

  /// Asserts that the type of this value is `IO`.
  #[inline(always)]
  pub fn as_io(self) {
    assert_eq!(self.ty(), ExtTy::IO);
  }
}

/// The type of an external value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum ExtTy {
  /// A 32-bit unsigned integer; the payload is simply the numeric value.
  u32,
  /// A 32-bit float; the payload is simply the bits of the float.
  f32,
  /// An IO handle, denoting the ability to do IO; the payload is always zero.
  IO,
}

/// A reference to an external function. The lower 15 bits are an [`ExtFnKind`],
/// determining the logic of the function, and the top bit denotes whether the
/// parameters to the function have been swapped.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtFn(u16);

impl ExtFn {
  const SWAP_BIT: u16 = 0x8000;

  #[inline(always)]
  pub fn bits(&self) -> u16 {
    self.0
  }

  #[inline(always)]
  pub fn kind(&self) -> ExtFnKind {
    unsafe { transmute::<u16, ExtFnKind>(self.0 & !Self::SWAP_BIT) }
  }

  #[inline(always)]
  pub fn is_swapped(&self) -> bool {
    (self.0 >> 15) == 1
  }

  #[inline(always)]
  pub fn swap(&self) -> Self {
    Self(self.0 ^ Self::SWAP_BIT)
  }

  /// Calls this function with the provided arguments.
  #[inline]
  pub fn call(self, a: ExtVal, b: ExtVal) -> ExtVal {
    if self.is_swapped() {
      self.kind().call(b, a)
    } else {
      self.kind().call(a, b)
    }
  }
}

impl From<ExtFnKind> for ExtFn {
  fn from(kind: ExtFnKind) -> Self {
    Self(kind as u16)
  }
}

macro_rules! ext_fns {
  ($($name:ident),* $(,)?) => {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(u16)]
    pub enum ExtFnKind {
      $($name,)*
    }

    impl ExtFnKind {
      pub const ALL: &'static [Self] = &[$(Self::$name),*];
      #[allow(clippy::should_implement_trait)]
      pub fn from_str(str: &str) -> Option<Self> {
        match str {
          $(stringify!($name) => Some(Self::$name),)*
          _ => None,
        }
      }
    }
  }
}

ext_fns! {
  seq,

  add,
  sub,
  mul,
  div,
  rem,

  eq,
  ne,
  lt,
  le,

  u32_shl,
  u32_shr,
  u32_and,
  u32_or,
  u32_xor,

  io_print_char,
  io_print_byte,
  io_flush,
  io_read_byte,
}

impl ExtFnKind {
  #[inline]
  fn call(self, a: ExtVal, b: ExtVal) -> ExtVal {
    use ExtFnKind::*;
    match self {
      seq => a,

      add => numeric_op(a, b, u32::wrapping_add, f32::add),
      sub => numeric_op(a, b, u32::wrapping_sub, f32::sub),
      mul => numeric_op(a, b, u32::wrapping_mul, f32::mul),
      div => numeric_op(a, b, u32::wrapping_div, f32::div),
      rem => numeric_op(a, b, u32::wrapping_rem, f32::rem_euclid),

      eq => comparison(a, b, |a, b| a == b, |a, b| a == b),
      ne => comparison(a, b, |a, b| a != b, |a, b| a != b),
      lt => comparison(a, b, |a, b| a < b, |a, b| a < b),
      le => comparison(a, b, |a, b| a <= b, |a, b| a <= b),

      u32_shl => ExtVal::new_u32(a.as_u32().wrapping_shl(b.as_u32())),
      u32_shr => ExtVal::new_u32(a.as_u32().wrapping_shr(b.as_u32())),
      u32_and => ExtVal::new_u32(a.as_u32() & b.as_u32()),
      u32_or => ExtVal::new_u32(a.as_u32() | b.as_u32()),
      u32_xor => ExtVal::new_u32(a.as_u32() ^ b.as_u32()),

      io_print_char => {
        a.as_io();
        print!("{}", char::try_from(b.as_u32()).unwrap());
        ExtVal::IO
      }
      io_print_byte => {
        a.as_io();
        io::stdout().write_all(&[b.as_u32() as u8]).unwrap();
        ExtVal::IO
      }
      io_flush => {
        a.as_io();
        io::stdout().flush().unwrap();
        ExtVal::IO
      }
      io_read_byte => {
        a.as_io();
        let default = b.as_u32() as u8;
        let mut buf = [default];
        _ = io::stdin().read(&mut buf).unwrap();
        ExtVal::new_u32(buf[0] as u32)
      }
    }
  }
}

fn numeric_op(
  a: ExtVal,
  b: ExtVal,
  f_u32: fn(u32, u32) -> u32,
  f_f32: fn(f32, f32) -> f32,
) -> ExtVal {
  match (a.ty(), b.ty()) {
    (ExtTy::u32, ExtTy::u32) => ExtVal::new_u32(f_u32(a.as_u32(), b.as_u32())),
    (ExtTy::f32, ExtTy::f32) => ExtVal::new_f32(f_f32(a.as_f32(), b.as_f32())),
    (ExtTy::u32, ExtTy::f32) => ExtVal::new_f32(f_f32(a.as_u32() as f32, b.as_f32())),
    (ExtTy::f32, ExtTy::u32) => ExtVal::new_f32(f_f32(a.as_f32(), b.as_u32() as f32)),
    _ => unimplemented!(),
  }
}

fn comparison(
  a: ExtVal,
  b: ExtVal,
  f_u32: fn(u32, u32) -> bool,
  f_f32: fn(f32, f32) -> bool,
) -> ExtVal {
  ExtVal::new_u32(u32::from(match (a.ty(), b.ty()) {
    (ExtTy::u32, ExtTy::u32) => f_u32(a.as_u32(), b.as_u32()),
    (ExtTy::f32, ExtTy::f32) => f_f32(a.as_f32(), b.as_f32()),
    (ExtTy::u32, ExtTy::f32) => f_f32(a.as_u32() as f32, b.as_f32()),
    (ExtTy::f32, ExtTy::u32) => f_f32(a.as_f32(), b.as_u32() as f32),
    _ => unimplemented!(),
  }))
}

impl Debug for ExtVal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty() {
      ExtTy::u32 => write!(f, "u32({})", self.payload()),
      ExtTy::f32 => write!(f, "f32({:?})", self.as_f32()),
      ExtTy::IO => write!(f, "IO"),
    }
  }
}

impl Debug for ExtFn {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?}{}", self.kind(), if self.is_swapped() { "$" } else { "" })
  }
}
