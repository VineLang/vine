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

    let as_n32 = move |x: ExtVal<'ivm>| x.as_ty(&n32);
    let as_f32 = move |x: ExtVal<'ivm>| f32::from_bits(x.as_ty(&f32));
    let new_n32 = move |x: u32| ExtVal::new(n32, x);
    let new_f32 = move |x: f32| ExtVal::new(f32, x.to_bits());
    let new_bool = move |x: bool| new_n32(x as u32);

    define_ext_fns!(self, extrinsics,
      "seq" => |a, _b| a,

      "n32_add" => |a, b| new_n32(as_n32(a).wrapping_add(as_n32(b))),
      "n32_sub" => |a, b| new_n32(as_n32(a).wrapping_sub(as_n32(b))),
      "n32_mul" => |a, b| new_n32(as_n32(a).wrapping_mul(as_n32(b))),
      "n32_div" => |a, b| new_n32(as_n32(a).wrapping_div(as_n32(b))),
      "n32_rem" => |a, b| new_n32(as_n32(a).wrapping_rem(as_n32(b))),

      "n32_eq" => |a, b| new_bool(as_n32(a) == as_n32(b)),
      "n32_ne" => |a, b| new_bool(as_n32(a) != as_n32(b)),
      "n32_lt" => |a, b| new_bool(as_n32(a) < as_n32(b)),
      "n32_le" => |a, b| new_bool(as_n32(a) <= as_n32(b)),

      "n32_shl" => |a, b| new_n32(as_n32(a).wrapping_shl(as_n32(b))),
      "n32_shr" => |a, b| new_n32(as_n32(a).wrapping_shr(as_n32(b))),
      "n32_rotl" => |a, b| new_n32(as_n32(a).rotate_left(as_n32(b))),
      "n32_rotr" => |a, b| new_n32(as_n32(a).rotate_right(as_n32(b))),

      "n32_and" => |a, b| new_n32(as_n32(a) & as_n32(b)),
      "n32_or" => |a, b| new_n32(as_n32(a) | as_n32(b)),
      "n32_xor" => |a, b| new_n32(as_n32(a) ^ as_n32(b)),

      "n32_add_high" => |a, b| new_n32((((as_n32(a) as u64) + (as_n32(b) as u64)) >> 32) as u32),
      "n32_mul_high" => |a, b| new_n32((((as_n32(a) as u64) * (as_n32(b) as u64)) >> 32) as u32),

      "f32_add" => |a, b| new_f32(as_f32(a).add(as_f32(b))),
      "f32_sub" => |a, b| new_f32(as_f32(a).sub(as_f32(b))),
      "f32_mul" => |a, b| new_f32(as_f32(a).mul(as_f32(b))),
      "f32_div" => |a, b| new_f32(as_f32(a).div(as_f32(b))),
      "f32_rem" => |a, b| new_f32(as_f32(a).rem_euclid(as_f32(b))),

      "f32_eq" => |a, b| new_bool(as_f32(a) == as_f32(b)),
      "f32_ne" => |a, b| new_bool(as_f32(a) != as_f32(b)),
      "f32_lt" => |a, b| new_bool(as_f32(a) < as_f32(b)),
      "f32_le" => |a, b| new_bool(as_f32(a) <= as_f32(b)),

      "n32_to_f32" => |a, _b| new_f32(as_n32(a) as f32),

      "io_print_char" => |a, b| {
        a.as_ty(&io);
        print!("{}", char::try_from(as_n32(b)).unwrap());
        ExtVal::new(io, 0)
      },
      "io_print_byte" => |a, b| {
        a.as_ty(&io);
        io::stdout().write_all(&[as_n32(b) as u8]).unwrap();
        ExtVal::new(io, 0)
      },
      "io_flush" => |a, _b| {
        a.as_ty(&io);
        io::stdout().flush().unwrap();
        ExtVal::new(io, 0)
      },
      "io_read_byte" => |a, b| {
        a.as_ty(&io);
        let default = as_n32(b) as u8;
        let mut buf = [default];
        _ = io::stdin().read(&mut buf).unwrap();
        new_n32(buf[0] as u32)
      }
    );
  }
}
