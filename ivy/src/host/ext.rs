use std::{
  io::{self, Read, Write},
  ops::{Add, Div, Mul, Sub},
};

use ivm::{
  ext::{ExtFn, ExtTy, ExtTyId, Extrinsics},
  port::Port,
};

use super::Host;

macro_rules! register_ext_fns {
  ($self:ident, $ext:ident, $($rest:tt)*) => {
    register_ext_fns!(@recurse $self, $ext, $($rest)* ,);
  };

  (@recurse $self:ident, $ext:ident,
    $name:expr => |$a:ident : $a_ty:ident| -> $c_ty:ident $body:block,
    $($rest:tt)*
  ) => {
    $self.register_ext_fn($name.into(), $ext.new_split_ext_fn(move |ivm, $a, out0, out1| {
      let $a = $a_ty.from_ext_val($a).unwrap();
      let res = $c_ty.into_ext_val({ $body });
      ivm.link_wire(out0, Port::ERASE);
      ivm.link_wire(out1, Port::new_ext_val(res));
    }));

    register_ext_fns!(@recurse $self, $ext, $($rest)*);
  };

  (@recurse $self:ident, $ext:ident,
    $name:expr => |$a:ident : $a_ty:ident, $b:ident : $b_ty:ident| -> $c_ty:ident $body:block,
    $($rest:tt)*
  ) => {
    $self.register_ext_fn($name.into(), $ext.new_merge_ext_fn(move |ivm, $a, $b, out| {
      let $a = $a_ty.from_ext_val($a).unwrap();
      let $b = $b_ty.from_ext_val($b).unwrap();
      let c = $c_ty.into_ext_val({ $body });
      ivm.link_wire(out, Port::new_ext_val(c));
    }));

    register_ext_fns!(@recurse $self, $ext, $($rest)*);
  };

  (@recurse $self:ident, $ext:ident,
    $name:expr => |$a:ident : $a_ty:ident| -> ( $b_ty:ident , $c_ty:ident ) $body:block,
    $($rest:tt)*
  ) => {
    $self.register_ext_fn($name.into(), $ext.new_split_ext_fn(move |ivm, $a, out0, out1| {
      let $a = $a_ty.from_ext_val($a).unwrap();
      let (b, c) = { $body };
      let b = $b_ty.into_ext_val(b);
      let c = $c_ty.into_ext_val(c);
      ivm.link_wire(out0, Port::new_ext_val(b));
      ivm.link_wire(out1, Port::new_ext_val(c));
    }));

    register_ext_fns!(@recurse $self, $ext, $($rest)*);
  };

  (@recurse $self:ident, $ext:ident,) => {};
}

impl<'ivm> Host<'ivm> {
  pub fn register_ext_fn(&mut self, name: String, f: ExtFn<'ivm>) {
    self.ext_fns.insert(name.clone(), f);
    self.reverse_ext_fns.insert(f, name);
  }

  pub fn register_ext_ty_id(&mut self, name: String, ty_id: ExtTyId<'ivm>) {
    self.ext_tys.insert(name.clone(), ty_id);
    self.reverse_ext_tys.insert(ty_id, name);
  }

  fn register_n32_ext_ty(&mut self, extrinsics: &mut Extrinsics<'ivm>) -> impl ExtTy<'ivm, u32> {
    let n32_ty = extrinsics.n32_ext_ty();
    self.register_ext_ty_id("N32".into(), n32_ty.ty_id());
    n32_ty
  }

  fn register_f32_ext_ty(&mut self, extrinsics: &mut Extrinsics<'ivm>) -> impl ExtTy<'ivm, f32> {
    let f32_ty = extrinsics.new_ext_ty(false);
    self.register_ext_ty_id("F32".into(), f32_ty.ty_id());
    f32_ty
  }

  fn register_io_ext_ty(&mut self, extrinsics: &mut Extrinsics<'ivm>) -> impl ExtTy<'ivm, ()> {
    let io_ty = extrinsics.new_ext_ty(false);
    self.register_ext_ty_id("IO".into(), io_ty.ty_id());
    io_ty
  }

  pub fn register_default_extrinsics(&mut self, extrinsics: &mut Extrinsics<'ivm>) {
    let n32 = self.register_n32_ext_ty(extrinsics);
    let f32 = self.register_f32_ext_ty(extrinsics);
    let io = self.register_io_ext_ty(extrinsics);

    self.register_ext_fn(
      "seq".into(),
      extrinsics.new_merge_ext_fn(move |ivm, a, _b, out| ivm.link_wire(out, Port::new_ext_val(a))),
    );

    register_ext_fns!(self, extrinsics,
      "n32_add" => |a: n32, b: n32| -> n32 { a.wrapping_add(b) },
      "n32_sub" => |a: n32, b: n32| -> n32 { a.wrapping_sub(b) },
      "n32_mul" => |a: n32, b: n32| -> n32 { a.wrapping_mul(b) },
      "n32_div" => |a: n32, b: n32| -> n32 { a.wrapping_div(b) },
      "n32_rem" => |a: n32, b: n32| -> n32 { a.wrapping_rem(b) },

      "n32_eq" => |a: n32, b: n32| -> n32 { (a == b) as u32 },
      "n32_ne" => |a: n32, b: n32| -> n32 { (a != b) as u32 },
      "n32_lt" => |a: n32, b: n32| -> n32 { (a < b) as u32 },
      "n32_le" => |a: n32, b: n32| -> n32 { (a <= b) as u32 },

      "n32_shl" => |a: n32, b: n32| -> n32 { a.wrapping_shl(b) },
      "n32_shr" => |a: n32, b: n32| -> n32 { a.wrapping_shr(b) },
      "n32_rotl" => |a: n32, b: n32| -> n32 { a.rotate_left(b) },
      "n32_rotr" => |a: n32, b: n32| -> n32 { a.rotate_right(b) },

      "n32_and" => |a: n32, b: n32| -> n32 { a & b },
      "n32_or" => |a: n32, b: n32| -> n32 { a | b },
      "n32_xor" => |a: n32, b: n32| -> n32 { a ^ b },

      "n32_add_high" => |a: n32, b: n32| -> n32 { (((a as u64) + (b as u64)) >> 32) as u32 },
      "n32_mul_high" => |a: n32, b: n32| -> n32 { (((a as u64) * (b as u64)) >> 32) as u32 },

      "f32_add" => |a: f32, b: f32| -> f32 { a.add(b) },
      "f32_sub" => |a: f32, b: f32| -> f32 { a.sub(b) },
      "f32_mul" => |a: f32, b: f32| -> f32 { a.mul(b) },
      "f32_div" => |a: f32, b: f32| -> f32 { a.div(b) },
      "f32_rem" => |a: f32, b: f32| -> f32 { a.rem_euclid(b) },

      "f32_eq" => |a: f32, b: f32| -> n32 { (a == b) as u32 },
      "f32_ne" => |a: f32, b: f32| -> n32 { (a != b) as u32 },
      "f32_lt" => |a: f32, b: f32| -> n32 { (a < b) as u32 },
      "f32_le" => |a: f32, b: f32| -> n32 { (a <= b) as u32 },

      "n32_to_f32" => |a: n32| -> f32 { a as f32 },
      "f32_to_n32" => |a: f32| -> n32 { a as u32 },
      "f32_to_bits" => |a: f32| -> n32 { a.to_bits() },
      "f32_from_bits" => |a: n32| -> f32 { f32::from_bits(a) },

      "i32_div" => |a: n32, b: n32| -> n32 { (a as i32 / b as i32) as u32 },
      "i32_rem" => |a: n32, b: n32| -> n32 { (a as i32 % b as i32) as u32 },
      "i32_shr" => |a: n32, b: n32| -> n32 { (a as i32).wrapping_shr(b) as u32 },
      "i32_lt" => |a: n32, b: n32| -> n32 { ((a as i32) < (b as i32)) as u32 },
      "i32_le" => |a: n32, b: n32| -> n32 { ((a as i32) <= (b as i32)) as u32 },

      "io_print_char" => |_io: io, b: n32| -> io {
        print!("{}", char::try_from(b).unwrap());
      },
      "io_print_byte" => |_io: io, b: n32| -> io {
        io::stdout().write_all(&[b as u8]).unwrap();
      },
      "io_flush" => |_io: io| -> io {
        io::stdout().flush().unwrap();
      },
      "io_read_byte" => |_io: io| -> (n32, io) {
        let mut buf = [0];
        _ = io::stdin().read(&mut buf).unwrap();
        (buf[0] as u32, ())
      }
    );
  }
}
