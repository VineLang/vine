use std::marker::PhantomData;

use crate::register::Register;

pub trait Define<I, O, W> {
  fn define(&mut self, name: &'static str, f: impl 'static + Send + Sync + Fn(I) -> O);
}

#[allow(nonstandard_style)]
fn Fn<R: Define<I, O, W>, I, O, W>(
  name: &'static str,
  f: impl 'static + Send + Sync + Fn(I) -> O,
) -> impl Register<R> {
  struct Def<F, I, O, W>(&'static str, F, PhantomData<(I, O, W)>);

  impl<F: 'static + Send + Sync + Fn(I) -> O, I, O, R: Define<I, O, W>, W> Register<R>
    for Def<F, I, O, W>
  {
    fn register(self, registry: &mut R) {
      registry.define(self.0, self.1);
    }
  }

  Def(name, f, PhantomData)
}

pub fn arithmetic<
  R,
  E: 'static + Send + Sync + Copy,
  W0,
  W1,
  W2,
  W3,
  W4,
  W5,
  W6,
  W7,
  W8,
  W9,
  W10,
  W11,
  W12,
  W13,
  W14,
  W15,
  W16,
  W17,
  W18,
>(
  err: E,
) -> impl Register<R>
where
  R: Define<(u32, u32), u32, W0>,
  R: Define<(u32, u32), bool, W1>,
  R: Define<(u32, u32), Result<u32, E>, W2>,
  R: Define<(f32, f32), f32, W3>,
  R: Define<f32, f32, W4>,
  R: Define<(f32, f32), bool, W5>,
  R: Define<u32, f32, W6>,
  R: Define<f32, u32, W7>,
  R: Define<f64, (f64, f64), W8>,
  R: Define<f64, (), W9>,
  R: Define<(f64, f64), f64, W10>,
  R: Define<f64, f64, W11>,
  R: Define<(f64, f64), bool, W12>,
  R: Define<(u32, u32), f64, W13>,
  R: Define<f64, (u32, u32), W14>,
  R: Define<f64, u32, W15>,
  R: Define<u32, f64, W16>,
  R: Define<f32, f64, W17>,
  R: Define<f64, f32, W18>,
{
  // u64 to/from (lo: u32, hi: u32) halves
  let u64_to_parts = |x: u64| (x as u32, (x >> 32) as u32);
  let u64_from_parts = |lo: u32, hi: u32| ((hi as u64) << 32) | (lo as u64);

  (
    (
      Fn("vi:n32:add", |(a, b): (u32, u32)| a.wrapping_add(b)),
      Fn("vi:n32:sub", |(a, b): (u32, u32)| a.wrapping_sub(b)),
      Fn("vi:n32:mul", |(a, b): (u32, u32)| a.wrapping_mul(b)),
      Fn("vi:n32:div", move |(a, b): (u32, u32)| a.checked_div(b).ok_or(err)),
      Fn("vi:n32:rem", move |(a, b): (u32, u32)| a.checked_rem(b).ok_or(err)),
    ),
    (
      Fn("vi:n32:eq", |(a, b): (u32, u32)| a == b),
      Fn("vi:n32:ne", |(a, b): (u32, u32)| a != b),
      Fn("vi:n32:lt", |(a, b): (u32, u32)| a < b),
      Fn("vi:n32:le", |(a, b): (u32, u32)| a <= b),
    ),
    (
      Fn("vi:n32:shl", |(a, b): (u32, u32)| a.wrapping_shl(b)),
      Fn("vi:n32:shr", |(a, b): (u32, u32)| a.wrapping_shr(b)),
      Fn("vi:n32:rotl", |(a, b): (u32, u32)| a.rotate_left(b)),
      Fn("vi:n32:rotr", |(a, b): (u32, u32)| a.rotate_right(b)),
    ),
    (
      Fn("vi:n32:and", |(a, b): (u32, u32)| a & b),
      Fn("vi:n32:or", |(a, b): (u32, u32)| a | b),
      Fn("vi:n32:xor", |(a, b): (u32, u32)| a ^ b),
    ),
    (
      Fn("vi:n32:add_high", |(a, b): (u32, u32)| (((a as u64) + (b as u64)) >> 32) as u32),
      Fn("vi:n32:mul_high", |(a, b): (u32, u32)| (((a as u64) * (b as u64)) >> 32) as u32),
    ),
    (
      Fn("vi:f32:add", |(a, b): (f32, f32)| a + b),
      Fn("vi:f32:sub", |(a, b): (f32, f32)| a - b),
      Fn("vi:f32:mul", |(a, b): (f32, f32)| a * b),
      Fn("vi:f32:div", |(a, b): (f32, f32)| a / b),
      Fn("vi:f32:rem", |(a, b): (f32, f32)| a.rem_euclid(b)),
      Fn("vi:f32:sqrt", |a: f32| a.sqrt()),
    ),
    (
      Fn("vi:f32:eq", |(a, b): (f32, f32)| a == b),
      Fn("vi:f32:ne", |(a, b): (f32, f32)| a != b),
      Fn("vi:f32:lt", |(a, b): (f32, f32)| a < b),
      Fn("vi:f32:le", |(a, b): (f32, f32)| a <= b),
    ),
    (
      Fn("vi:n32:to_f32", |a: u32| a as f32),
      Fn("vi:f32:to_n32", |a: f32| a as u32),
      Fn("vi:i32:to_f32", |a: u32| a as i32 as f32),
      Fn("vi:f32:to_i32", |a: f32| a as i32 as u32),
      Fn("vi:f32:to_bits", |a: f32| {
        // Use unique deterministic bit pattern for NaN values.
        if a.is_nan() { 0x7FC00000 } else { a.to_bits() }
      }),
      Fn("vi:f32:from_bits", |a: u32| f32::from_bits(a)),
    ),
    (
      Fn("vi:i32:div", move |(a, b): (u32, u32)| {
        Ok((a as i32).checked_div(b as i32).ok_or(err)? as u32)
      }),
      Fn("vi:i32:rem", move |(a, b): (u32, u32)| {
        Ok((a as i32).checked_rem(b as i32).ok_or(err)? as u32)
      }),
      Fn("vi:i32:shr", |(a, b): (u32, u32)| (a as i32).wrapping_shr(b) as u32),
      Fn("vi:i32:lt", |(a, b): (u32, u32)| (a as i32) < b as i32),
      Fn("vi:i32:le", |(a, b): (u32, u32)| a as i32 <= b as i32),
    ),
    (
      Fn("vi:f64:fork", |f: f64| -> (f64, f64) { (f, f) }),
      Fn("vi:f64:drop", |_: f64| {}), //
    ),
    (
      Fn("vi:f64:add", |(a, b): (f64, f64)| a + b),
      Fn("vi:f64:sub", |(a, b): (f64, f64)| a - b),
      Fn("vi:f64:mul", |(a, b): (f64, f64)| a * b),
      Fn("vi:f64:div", |(a, b): (f64, f64)| a / b),
      Fn("vi:f64:rem", |(a, b): (f64, f64)| a.rem_euclid(b)),
      Fn("vi:f64:sqrt", |a: f64| a.sqrt()),
    ),
    (
      Fn("vi:f64:eq", |(a, b): (f64, f64)| a == b),
      Fn("vi:f64:ne", |(a, b): (f64, f64)| a != b),
      Fn("vi:f64:lt", |(a, b): (f64, f64)| a < b),
      Fn("vi:f64:le", |(a, b): (f64, f64)| a <= b),
    ),
    (
      Fn("vi:n32:to_f64", |n: u32| n as f64),
      Fn("vi:f64:to_n32", |f: f64| f as u32),
      Fn("vi:i32:to_f64", |a: u32| a as i32 as f64),
      Fn("vi:f64:to_i32", |a: f64| a as i32 as u32),
      Fn("vi:f32:to_f64", |f: f32| f as f64),
      Fn("vi:f64:to_f32", |f: f64| f as f32),
      Fn("vi:n64:to_f64", move |(lo, hi): (u32, u32)| u64_from_parts(lo, hi) as f64),
      Fn("vi:f64:to_n64", move |f: f64| -> (u32, u32) { u64_to_parts(f as u64) }),
      Fn("vi:f64:to_bits", move |f: f64| -> (u32, u32) {
        // Use unique deterministic bit pattern for NaN values.
        if f.is_nan() { (0, 0x7FF80000) } else { u64_to_parts(f.to_bits()) }
      }),
      Fn("vi:f64:from_bits", move |(lo, hi): (u32, u32)| f64::from_bits(u64_from_parts(lo, hi))),
    ),
  )
}
