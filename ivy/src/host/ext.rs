use std::{
  io::{self, Read, Write},
  ops::{Add, Div, Mul, Sub},
};

use ivm::ext::{ExtFn, ExtTy, ExtVal, Extrinsics};

use super::Host;

macro_rules! define_ext_fns {
  ($self:ident, $ext:ident, $($name:expr => |$a:ident, $b:ident| $body:expr ),*) => {
    $($self.register_ext_fn($name.into(), $ext.register_ext_fn(move |[$a, $b]| [$body]));)*
  };
}

impl<'ivm> Host<'ivm> {
  pub fn register_ext_fn(&mut self, name: String, f: ExtFn<'ivm>) {
    self.ext_fns.insert(name.clone(), f);
    self.reverse_ext_fns.insert(f, name);
  }

  pub fn register_ext_ty(&mut self, name: String, ty: ExtTy<'ivm>) {
    self.ext_tys.insert(name.clone(), ty);
    self.reverse_ext_tys.insert(ty, name);
  }

  pub fn register_default_extrinsics(&mut self, extrinsics: &mut Extrinsics<'ivm>) {
    let n32 = extrinsics.register_n32_ext_ty();
    let f32 = extrinsics.register_light_ext_ty();
    let io = extrinsics.register_light_ext_ty();
    self.register_ext_ty("N32".into(), n32);
    self.register_ext_ty("F32".into(), f32);
    self.register_ext_ty("IO".into(), io);
    define_ext_fns!(self, extrinsics,
      "seq" => |a, _b| a,

      "add" => |a, b| numeric_op(n32, f32, a, b, u32::wrapping_add, f32::add),
      "sub" => |a, b| numeric_op(n32, f32, a, b, u32::wrapping_sub, f32::sub),
      "mul" => |a, b| numeric_op(n32, f32, a, b, u32::wrapping_mul, f32::mul),
      "div" => |a, b| numeric_op(n32, f32, a, b, u32::wrapping_div, f32::div),
      "rem" => |a, b| numeric_op(n32, f32, a, b, u32::wrapping_rem, f32::rem_euclid),

      "eq" => |a, b| comparison(n32, f32, a, b, |a, b| a == b, |a, b| a == b),
      "ne" => |a, b| comparison(n32, f32, a, b, |a, b| a != b, |a, b| a != b),
      "lt" => |a, b| comparison(n32, f32, a, b, |a, b| a < b, |a, b| a < b),
      "le" => |a, b| comparison(n32, f32, a, b, |a, b| a <= b, |a, b| a <= b),

      "n32_shl" => |a, b| ExtVal::new(n32, a.as_ty(&n32).wrapping_shl(b.as_ty(&n32))),
      "n32_shr" => |a, b| ExtVal::new(n32, a.as_ty(&n32).wrapping_shr(b.as_ty(&n32))),
      "n32_rotl" => |a, b| ExtVal::new(n32, a.as_ty(&n32).rotate_left(b.as_ty(&n32))),
      "n32_rotr" => |a, b| ExtVal::new(n32, a.as_ty(&n32).rotate_right(b.as_ty(&n32))),

      "n32_and" => |a, b| ExtVal::new(n32, a.as_ty(&n32) & b.as_ty(&n32)),
      "n32_or" => |a, b| ExtVal::new(n32, a.as_ty(&n32) | b.as_ty(&n32)),
      "n32_xor" => |a, b| ExtVal::new(n32, a.as_ty(&n32) ^ b.as_ty(&n32)),

      "n32_add_high" => |a, b| ExtVal::new(n32, (((a.as_ty(&n32) as u64) + (b.as_ty(&n32) as u64)) >> 32) as u32),
      "n32_mul_high" => |a, b| ExtVal::new(n32, (((a.as_ty(&n32) as u64) * (b.as_ty(&n32) as u64)) >> 32) as u32),

      "io_print_char" => |a, b| {
        a.as_ty(&io);
        print!("{}", char::try_from(b.as_ty(&n32)).unwrap());
        ExtVal::new(io, 0)
      },
      "io_print_byte" => |a, b| {
        a.as_ty(&io);
        io::stdout().write_all(&[b.as_ty(&n32) as u8]).unwrap();
        ExtVal::new(io, 0)
      },
      "io_flush" => |a, _b| {
        a.as_ty(&io);
        io::stdout().flush().unwrap();
        ExtVal::new(io, 0)
      },
      "io_read_byte" => |a, b| {
        a.as_ty(&io);
        let default = b.as_ty(&n32) as u8;
        let mut buf = [default];
        _ = io::stdin().read(&mut buf).unwrap();
        ExtVal::new(n32, buf[0] as u32)
      }
    );

    enum NumericType {
      F32,
      N32,
    }

    fn ty_to_numeric_type<'ivm>(
      val: ExtVal<'ivm>,
      n32: ExtTy<'ivm>,
      f32: ExtTy<'ivm>,
    ) -> Option<NumericType> {
      if val.ty() == n32 {
        Some(NumericType::N32)
      } else if val.ty() == f32 {
        Some(NumericType::F32)
      } else {
        None
      }
    }

    fn numeric_op<'ivm>(
      n32: ExtTy<'ivm>,
      f32: ExtTy<'ivm>,
      a: ExtVal<'ivm>,
      b: ExtVal<'ivm>,
      f_n32: fn(u32, u32) -> u32,
      f_f32: fn(f32, f32) -> f32,
    ) -> ExtVal<'ivm> {
      match (ty_to_numeric_type(a, n32, f32), ty_to_numeric_type(b, n32, f32)) {
        (Some(NumericType::N32), Some(NumericType::N32)) => {
          ExtVal::new(n32, f_n32(a.as_ty(&n32), b.as_ty(&n32)))
        }
        (Some(NumericType::F32), Some(NumericType::F32)) => ExtVal::new(
          f32,
          f_f32(f32::from_bits(a.as_ty(&f32)), f32::from_bits(b.as_ty(&f32))).to_bits(),
        ),
        (Some(NumericType::N32), Some(NumericType::F32)) => {
          ExtVal::new(f32, f_f32(a.as_ty(&n32) as f32, f32::from_bits(b.as_ty(&f32))).to_bits())
        }
        (Some(NumericType::F32), Some(NumericType::N32)) => {
          ExtVal::new(f32, f_f32(f32::from_bits(a.as_ty(&f32)), b.as_ty(&n32) as f32).to_bits())
        }
        _ => unimplemented!(),
      }
    }

    fn comparison<'ivm>(
      n32: ExtTy<'ivm>,
      f32: ExtTy<'ivm>,
      a: ExtVal<'ivm>,
      b: ExtVal<'ivm>,
      f_u32: fn(u32, u32) -> bool,
      f_f32: fn(f32, f32) -> bool,
    ) -> ExtVal<'ivm> {
      ExtVal::new(
        n32,
        u32::from(match (ty_to_numeric_type(a, n32, f32), ty_to_numeric_type(b, n32, f32)) {
          (Some(NumericType::N32), Some(NumericType::N32)) => f_u32(a.as_ty(&n32), b.as_ty(&n32)),
          (Some(NumericType::F32), Some(NumericType::F32)) => {
            f_f32(f32::from_bits(a.as_ty(&f32)), f32::from_bits(b.as_ty(&f32)))
          }
          (Some(NumericType::N32), Some(NumericType::F32)) => {
            f_f32(a.as_ty(&n32) as f32, f32::from_bits(b.as_ty(&f32)))
          }
          (Some(NumericType::F32), Some(NumericType::N32)) => {
            f_f32(f32::from_bits(a.as_ty(&f32)), b.as_ty(&n32) as f32)
          }
          _ => unimplemented!(),
        }),
      )
    }
  }
}
