use std::{
  io::{self, Read, Write},
  ops::{Add, Div, Mul, Sub},
};

use ivm::{
  ext::{ExtFn, ExtTy, ExtVal, Extrinsics},
  port::Port,
};

use super::Host;

macro_rules! register_merge_ext_fns {
  ($self:ident, $ext:ident, $($name:expr => |$a:ident: $a_ty:ident, $b:ident: $b_ty:ident| -> $c_ty:ident $body:expr ),*) => {
    $($self.register_ext_fn($name.into(), $ext.new_merge_ext_fn(move |ivm, $a, $b, out| {
      #[allow(unused)]
      let $a = $a_ty.from_ext_val($a);
      #[allow(unused)]
      let $b = $b_ty.from_ext_val($b);
      let c = $c_ty.into_ext_val({ $body });
      ivm.link_wire(out, Port::new_ext_val(c));
    }));)*
  };
}

macro_rules! register_split_ext_fns {
  ($self:ident, $ext:ident, $($name:expr => |$a:ident: $a_ty:ident| -> ( $b_ty:ident , $c_ty:ident ) $body:expr ),*) => {
    $($self.register_ext_fn($name.into(), $ext.new_split_ext_fn(move |ivm, $a, out0, out1| {
      #[allow(unused)]
      let $a = $a_ty.from_ext_val($a);
      let (b, c) = { $body };
      let b = $b_ty.into_ext_val(b);
      let c = $c_ty.into_ext_val(c);
      ivm.link_wire(out0, Port::new_ext_val(b));
      ivm.link_wire(out1, Port::new_ext_val(c));
    }));)*
  };
}

/// Convenience trait for using [`ExtTy`]'s in type-annotation-looking positions
/// inside [`register_merge_ext_fns!`] and [`register_split_ext_fns!`].
///
/// This defines how to convert an [`ExtVal<'ivm>`] to and from a `T`.
#[allow(clippy::wrong_self_convention)]
trait ExtTyAnnotation<'ivm, T>: Copy {
  fn from_ext_val(&self, ext_val: ExtVal<'ivm>) -> T;
  fn into_ext_val(&self, val: T) -> ExtVal<'ivm>;
}

impl<'ivm, F, G, T> ExtTyAnnotation<'ivm, T> for (F, G)
where
  F: Fn(ExtVal<'ivm>) -> T + Copy,
  G: Fn(T) -> ExtVal<'ivm> + Copy,
{
  fn from_ext_val(&self, ext_val: ExtVal<'ivm>) -> T {
    (self.0)(ext_val)
  }

  fn into_ext_val(&self, val: T) -> ExtVal<'ivm> {
    (self.1)(val)
  }
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

  fn register_n32_ext_ty(
    &mut self,
    extrinsics: &mut Extrinsics<'ivm>,
  ) -> impl ExtTyAnnotation<'ivm, u32> + 'ivm {
    let n32_ty = extrinsics.n32_ext_ty();
    self.register_ext_ty("N32".into(), n32_ty);
    let as_n32 = move |x: ExtVal<'ivm>| x.as_ty(&n32_ty) as u32;
    let new_n32 = move |x: u32| ExtVal::new(n32_ty, x as u64);
    (as_n32, new_n32)
  }

  fn register_f32_ext_ty(
    &mut self,
    extrinsics: &mut Extrinsics<'ivm>,
  ) -> impl ExtTyAnnotation<'ivm, f32> + 'ivm {
    let f32_ty = extrinsics.new_ext_ty(false);
    self.register_ext_ty("F32".into(), f32_ty);
    let as_f32 = move |x: ExtVal<'ivm>| f32::from_bits(x.as_ty(&f32_ty) as u32);
    let new_f32 = move |x: f32| ExtVal::new(f32_ty, x.to_bits() as u64);
    (as_f32, new_f32)
  }

  fn register_io_ext_ty(
    &mut self,
    extrinsics: &mut Extrinsics<'ivm>,
  ) -> impl ExtTyAnnotation<'ivm, ()> + 'ivm {
    let io_ty = extrinsics.new_ext_ty(false);
    self.register_ext_ty("IO".into(), io_ty);
    let as_io = move |x: ExtVal<'ivm>| _ = x.as_ty(&io_ty);
    let new_io = move |()| ExtVal::new(io_ty, 0);
    (as_io, new_io)
  }

  pub fn register_default_extrinsics(&mut self, extrinsics: &mut Extrinsics<'ivm>) {
    let n32 = self.register_n32_ext_ty(extrinsics);
    let f32 = self.register_f32_ext_ty(extrinsics);
    let io = self.register_io_ext_ty(extrinsics);
    let any = (|x| x, |x| x);

    register_merge_ext_fns!(self, extrinsics,
      "seq" => |a: any, _b: any| -> any a,

      "n32_add" => |a: n32, b: n32| -> n32 a.wrapping_add(b),
      "n32_sub" => |a: n32, b: n32| -> n32 a.wrapping_sub(b),
      "n32_mul" => |a: n32, b: n32| -> n32 a.wrapping_mul(b),
      "n32_div" => |a: n32, b: n32| -> n32 a.wrapping_div(b),
      "n32_rem" => |a: n32, b: n32| -> n32 a.wrapping_rem(b),

      "n32_eq" => |a: n32, b: n32| -> n32 (a == b) as u32,
      "n32_ne" => |a: n32, b: n32| -> n32 (a != b) as u32,
      "n32_lt" => |a: n32, b: n32| -> n32 (a < b) as u32,
      "n32_le" => |a: n32, b: n32| -> n32 (a <= b) as u32,

      "n32_shl" => |a: n32, b: n32| -> n32 a.wrapping_shl(b),
      "n32_shr" => |a: n32, b: n32| -> n32 a.wrapping_shr(b),
      "n32_rotl" => |a: n32, b: n32| -> n32 a.rotate_left(b),
      "n32_rotr" => |a: n32, b: n32| -> n32 a.rotate_right(b),

      "n32_and" => |a: n32, b: n32| -> n32 a & b,
      "n32_or" => |a: n32, b: n32| -> n32 a | b,
      "n32_xor" => |a: n32, b: n32| -> n32 a ^ b,

      "n32_add_high" => |a: n32, b: n32| -> n32 (((a as u64) + (b as u64)) >> 32) as u32,
      "n32_mul_high" => |a: n32, b: n32| -> n32 (((a as u64) * (b as u64)) >> 32) as u32,

      "f32_add" => |a: f32, b: f32| -> f32 a.add(b),
      "f32_sub" => |a: f32, b: f32| -> f32 a.sub(b),
      "f32_mul" => |a: f32, b: f32| -> f32 a.mul(b),
      "f32_div" => |a: f32, b: f32| -> f32 a.div(b),
      "f32_rem" => |a: f32, b: f32| -> f32 a.rem_euclid(b),

      "f32_eq" => |a: f32, b: f32| -> n32 (a == b) as u32,
      "f32_ne" => |a: f32, b: f32| -> n32 (a != b) as u32,
      "f32_lt" => |a: f32, b: f32| -> n32 (a < b) as u32,
      "f32_le" => |a: f32, b: f32| -> n32 (a <= b) as u32,

      "n32_to_f32" => |a: n32, _b: any| -> f32 a as f32,
      "f32_to_n32" => |a: f32, _b: any| -> n32 a as u32,
      "f32_to_bits" => |a: f32, _b: any| -> n32 a.to_bits(),
      "f32_from_bits" => |a: n32, _b: any| -> f32 f32::from_bits(a),

      "i32_div" => |a: n32, b: n32| -> n32 (a as i32 / b as i32) as u32,
      "i32_rem" => |a: n32, b: n32| -> n32 (a as i32 % b as i32) as u32,
      "i32_shr" => |a: n32, b: n32| -> n32 (a as i32).wrapping_shr(b) as u32,
      "i32_lt" => |a: n32, b: n32| -> n32 ((a as i32) < (b as i32)) as u32,
      "i32_le" => |a: n32, b: n32| -> n32 ((a as i32) <= (b as i32)) as u32,

      "io_print_char" => |a: io, b: n32| -> io {
        print!("{}", char::try_from(b).unwrap());
      },
      "io_print_byte" => |a: io, b: n32| -> io {
        io::stdout().write_all(&[b as u8]).unwrap();
      },
      "io_flush" => |a: io, _b: any| -> io {
        io::stdout().flush().unwrap();
      }
    );

    register_split_ext_fns!(self, extrinsics,
      "io_read_byte" => |a: io| -> (n32, io) {
        let mut buf = [0];
        _ = io::stdin().read(&mut buf).unwrap();
        (buf[0] as u32, ())
      }
    );
  }
}
