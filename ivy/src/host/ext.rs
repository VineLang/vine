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
  (match ($self:ident, $ext:ident) { $(
    $name:expr => |$($param_name:ident : $param_ty:ident),*| $(-> $out:tt)? $body:block,
  )* }) => {
    $($self.register_ext_fn($name.into(),
      register_ext_fns!(@impl $ext, |$($param_name : $param_ty),*| $(-> $out)? $body)
    );)*
  };

  (@impl $ext:ident, |$a:ident : $a_ty:ident| $body:block) => {
    $ext.new_split_ext_fn(move |ivm, $a, out0, out1| {
      let $a = $a_ty.unwrap_ext_val($a);

      if let Some($a) = $a {
        let () = $body;
      } else {
        ivm.flags.ext_generic = true;
      }

      ivm.link_wire(out0, Port::ERASE);
      ivm.link_wire(out1, Port::ERASE);
    })
  };

  (@impl $ext:ident, |$a:ident : $a_ty:ident| -> $c_ty:ident $body:block) => {
    $ext.new_split_ext_fn(move |ivm, $a, out0, out1| {
      let (val0, val1) = (|| {
        let $a = $a_ty.unwrap_ext_val($a)?;
        let res = $c_ty.wrap_ext_val($body);
        Some((Port::ERASE, Port::new_ext_val(res)))
      })().unwrap_or_else(|| {
        ivm.flags.ext_generic = true;
        (Port::ERASE, Port::ERASE)
      });

      ivm.link_wire(out0, val0);
      ivm.link_wire(out1, val1);
    })
  };

  (@impl $ext:ident, |$a:ident : $a_ty:ident, $b:ident : $b_ty:ident| -> $c_ty:ident $body:block) => {
    $ext.new_merge_ext_fn(move |ivm, $a, $b, out| {
      let val = (|| {
        let $a = $a_ty.unwrap_ext_val($a)?;
        let $b = $b_ty.unwrap_ext_val($b)?;
        let res = $c_ty.wrap_ext_val($body);
        Some(Port::new_ext_val(res))
      })().unwrap_or_else(|| {
        ivm.flags.ext_generic = true;
        Port::ERASE
      });

      ivm.link_wire(out, val);
    })
  };

  (@impl $ext:ident, |$a:ident : $a_ty:ident| -> ($b_ty:ident, $c_ty:ident) $body:block) => {
    $ext.new_split_ext_fn(move |ivm, $a, out0, out1| {
      let (val0, val1) = (|| {
        let $a = $a_ty.unwrap_ext_val($a)?;
        let (b, c) = $body;
        let b = $b_ty.wrap_ext_val(b);
        let c = $c_ty.wrap_ext_val(c);
        Some((Port::new_ext_val(b), Port::new_ext_val(c)))
      })().unwrap_or_else(|| {
        ivm.flags.ext_generic = true;
        (Port::ERASE, Port::ERASE)
      });

      ivm.link_wire(out0, val0);
      ivm.link_wire(out1, val1);
    })
  };
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

  fn register_n32_ext_ty(&mut self, extrinsics: &mut Extrinsics<'ivm>) -> ExtTy<'ivm, u32> {
    let n32_ty = extrinsics.n32_ext_ty();
    self.register_ext_ty_id("N32".into(), n32_ty.ty_id());
    n32_ty
  }

  fn register_f32_ext_ty(&mut self, extrinsics: &mut Extrinsics<'ivm>) -> ExtTy<'ivm, f32> {
    let f32_ty = extrinsics.new_ext_ty();
    self.register_ext_ty_id("F32".into(), f32_ty.ty_id());
    f32_ty
  }

  fn register_io_ext_ty(&mut self, extrinsics: &mut Extrinsics<'ivm>) -> ExtTy<'ivm, ()> {
    let io_ty = extrinsics.new_ext_ty();
    self.register_ext_ty_id("IO".into(), io_ty.ty_id());
    io_ty
  }

  pub fn register_default_extrinsics(&mut self, extrinsics: &mut Extrinsics<'ivm>) {
    let n32 = self.register_n32_ext_ty(extrinsics);
    let f32 = self.register_f32_ext_ty(extrinsics);
    let io = self.register_io_ext_ty(extrinsics);

    register_ext_fns!(match (self, extrinsics) {
      "n32_add" => |a: n32, b: n32| -> n32 { a.wrapping_add(b) },
      "n32_sub" => |a: n32, b: n32| -> n32 { a.wrapping_sub(b) },
      "n32_mul" => |a: n32, b: n32| -> n32 { a.wrapping_mul(b) },
      "n32_div" => |a: n32, b: n32| -> n32 { a.checked_div(b)? },
      "n32_rem" => |a: n32, b: n32| -> n32 { a.checked_rem(b)? },

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

      "i32_div" => |a: n32, b: n32| -> n32 { (a as i32).checked_div(b as i32)? as u32 },
      "i32_rem" => |a: n32, b: n32| -> n32 { (a as i32).checked_rem(b as i32)? as u32 },
      "i32_shr" => |a: n32, b: n32| -> n32 { (a as i32).wrapping_shr(b) as u32 },
      "i32_lt" => |a: n32, b: n32| -> n32 { ((a as i32) < (b as i32)) as u32 },
      "i32_le" => |a: n32, b: n32| -> n32 { ((a as i32) <= (b as i32)) as u32 },

      "io_join" => |_io_a: io, _io_b: io| -> io {},
      "io_split" => |_io: io| -> (io, io) { ((), ()) },
      "io_ready" => |_io: io| -> (n32, io) { (1, ()) },

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
        let count = io::stdin().read(&mut buf).unwrap();
        let byte = if count == 0 { u32::MAX } else { buf[0] as u32 };
        (byte, ())
      },
      "io_read_char" => |_io: io| -> (n32, io) {
        let mut buf = [0];
        let count = io::stdin().read(&mut buf).unwrap();
        let c = match (count, buf[0].leading_ones()) {
          (0, _) => Some(u32::MAX),
          (_, 0) => Some(buf[0] as u32),
          (_, 2) => read_bytes_into_utf8_u32(buf[0], 1),
          (_, 3) => read_bytes_into_utf8_u32(buf[0], 2),
          (_, 4) => read_bytes_into_utf8_u32(buf[0], 3),
          _ => None,
        };

        (c.unwrap_or(char::REPLACEMENT_CHARACTER as u32), ())
      },
    });
  }
}

/// Reads `n` bytes from stdin, and returns the `u32` representation of the
/// UTF-8 codepoint composed of the `first_byte` and the 1-3 read bytes.
///
/// If less than `n` bytes are read, or if the bytes are invalid UTF-8, `None`
/// is returned.
fn read_bytes_into_utf8_u32(first_byte: u8, n: usize) -> Option<u32> {
  assert!((1..=3).contains(&n));

  let buf = &mut [0; 4][..(n + 1)];
  let count = io::stdin().read(&mut buf[1..]).unwrap();
  if count != n {
    return None;
  }

  buf[0] = first_byte;

  Some(str::from_utf8(buf).ok()?.chars().next()? as u32)
}
