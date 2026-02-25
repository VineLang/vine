use std::{
  fs::{File, OpenOptions},
  io::{self, Error, ErrorKind, Read, Write},
  ops::{Add, Div, Mul, Sub},
};

use ivm::{
  ext::{Boxed, ExtFn, ExtTy, ExtTyCast, ExtTyId, ExtVal, Extrinsics},
  port::Port,
};

use super::Host;

macro_rules! register_ext_fns {
  (match ($self:ident, $ext:ident) { $(
    $name:expr => |$($param_name:ident : $param_ty:ident),*| $(-> $out:tt)? $body:block,
  )* }) => {
    $($self.register_ext_fn($name,
      register_ext_fns!(@impl $ext, |$($param_name : $param_ty),*| $(-> $out)? $body)
    );)*
  };

  (@impl $ext:ident, |$a:ident : $a_ty:ident| $body:block) => {
    $ext.new_split_ext_fn(move |ivm, $a, out0, out1| {
      let $a = $a_ty.unwrap_ext_val($a);

      #[allow(unused)]
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
        #[allow(unused_mut)]
        let mut $a = $a_ty.unwrap_ext_val($a)?;
        #[allow(unused_mut)]
        let mut $b = $b_ty.unwrap_ext_val($b)?;
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
        #[allow(unused_mut)]
        let mut $a = $a_ty.unwrap_ext_val($a)?;
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

macro_rules! list {
  ($($expr:tt : $ty:expr),*) => {
    Boxed::from(vec![$($ty.wrap_ext_val($expr.into())),*])
  };
}

macro_rules! tuple {
  ($($expr:tt : $ty:expr),*) => {
    {
      let mut list = list![$($expr : $ty),*];
      list.reverse();
      list
    }
  };
}

macro_rules! untuple {
  ($expr:expr ; $($ty:ident),*) => {
    ($($ty.unwrap_ext_val($expr.pop()?)?),*)
  };
}

macro_rules! result_ok {
  ($n32:ident; $expr:tt : $ty:expr) => {
    tuple![false : $n32, $expr : $ty]
  };
}

macro_rules! result_err {
  ($n32:ident; $expr:tt : $ty:expr) => {
    tuple![true : $n32, $expr : $ty]
  };
}

impl<'ivm> Host<'ivm> {
  pub fn register_ext_fn(&mut self, name: impl Into<String>, f: ExtFn<'ivm>) {
    let name = name.into();
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

  fn register_ext_ty<T: ExtTyCast<'ivm>>(
    &mut self,
    name: &'static str,
    extrinsics: &mut Extrinsics<'ivm>,
  ) -> ExtTy<'ivm, T> {
    let ty = extrinsics.new_ext_ty();
    self.register_ext_ty_id(name.into(), ty.ty_id());
    ty
  }

  pub fn register_default_extrinsics(&mut self, extrinsics: &mut Extrinsics<'ivm>) {
    let n32 = self.register_n32_ext_ty(extrinsics);
    let f32 = self.register_ext_ty::<f32>("F32", extrinsics);
    let f64 = self.register_ext_ty::<f64>("F64", extrinsics);

    // u64 to/from (lo: u32, hi: u32) halves
    let u64_to_parts = |x: u64| (x as u32, (x >> 32) as u32);
    let u64_from_parts = |lo: u32, hi: u32| ((hi as u64) << 32) | (lo as u64);

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

      "f64_fork" => |f: f64| -> (f64, f64) { (f, f) },
      "f64_drop" => |f: f64| {},

      "f64_add" => |a: f64, b: f64| -> f64 { a + b },
      "f64_sub" => |a: f64, b: f64| -> f64 { a - b },
      "f64_mul" => |a: f64, b: f64| -> f64 { a * b },
      "f64_div" => |a: f64, b: f64| -> f64 { a / b },
      "f64_rem" => |a: f64, b: f64| -> f64 { a.rem_euclid(b) },

      "f64_eq" => |a: f64, b: f64| -> n32 { (a == b) as u32 },
      "f64_ne" => |a: f64, b: f64| -> n32 { (a != b) as u32 },
      "f64_lt" => |a: f64, b: f64| -> n32 { (a < b) as u32 },
      "f64_le" => |a: f64, b: f64| -> n32 { (a <= b) as u32 },

      "n32_to_f64" => |n: n32| -> f64 { n as f64 },
      "f64_to_n32" => |f: f64| -> n32 { f as u32 },
      "f32_to_f64" => |f: f32| -> f64 { f as f64 },
      "f64_to_f32" => |f: f64| -> f32 { f as f32 },
      "n64_to_f64" => |lo: n32, hi: n32| -> f64 { u64_from_parts(lo, hi) as f64 },
      "f64_to_n64" => |f: f64| -> (n32, n32) { u64_to_parts(f as u64) },
      "f64_to_bits" => |f: f64| -> (n32, n32) { u64_to_parts(f.to_bits()) },
      "f64_from_bits" => |lo: n32, hi: n32| -> f64 { f64::from_bits(u64_from_parts(lo, hi)) },
    });
  }

  pub fn register_runtime_extrinsics<R: Read, W: Write>(
    &mut self,
    extrinsics: &mut Extrinsics<'ivm>,
    args: &'ivm [String],
    io_input_fn: impl Copy + Fn() -> R + Send + Sync + 'ivm,
    io_output_fn: impl Copy + Fn() -> W + Send + Sync + 'ivm,
  ) {
    let n32 = extrinsics.n32_ext_ty();
    let list = self.register_ext_ty::<Boxed<Vec<ExtVal<'ivm>>>>("List", extrinsics);
    let io = self.register_ext_ty::<()>("IO", extrinsics);
    let file = self.register_ext_ty::<Boxed<File>>("File", extrinsics);
    let error = self.register_ext_ty::<Boxed<Error>>("File", extrinsics);

    self.register_ext_fn(
      "list_push",
      extrinsics.new_merge_ext_fn(move |ivm, l, el, out| {
        let Some(mut l) = list.unwrap_ext_val(l) else {
          ivm.flags.ext_generic = true;
          return;
        };

        l.push(el);

        ivm.link_wire(out, Port::new_ext_val(list.wrap_ext_val(l)));
      }),
    );

    self.register_ext_fn(
      "list_pop",
      extrinsics.new_split_ext_fn(move |ivm, l, out0, out1| {
        let Some(mut l) = list.unwrap_ext_val(l) else {
          ivm.flags.ext_generic = true;
          return;
        };

        let Some(el) = l.pop() else {
          ivm.flags.ext_generic = true;
          return;
        };

        ivm.link_wire(out0, Port::new_ext_val(el));
        ivm.link_wire(out1, Port::new_ext_val(list.wrap_ext_val(l)));
      }),
    );

    self.register_ext_fn(
      "list_drop",
      extrinsics.new_split_ext_fn(move |ivm, l, out0, out1| {
        match list.unwrap_ext_val(l) {
          Some(l) if !l.is_empty() => ivm.flags.ext_erase = true,
          None => ivm.flags.ext_generic = true,
          _ => {}
        }

        ivm.link_wire(out0, Port::ERASE);
        ivm.link_wire(out1, Port::ERASE);
      }),
    );

    register_ext_fns!(match (self, extrinsics) {
      "list_new" => |_unused: n32| -> list { Default::default() },
      "list_len" => |l: list| -> (n32, list) { (l.len() as u32, l) },

      "file_open" => |_io: io, path: list| -> list {
        let path = ext_val_as_str(n32, path.into_inner())?;
        let res = OpenOptions::new().read(true).append(true).open(path);
        let res = match res {
          Ok(f) => result_ok![n32; f: file],
          Err(e) => result_err![n32; e: error],
        };
        tuple![(): io, res: list]
      },
      "file_create" => |_io: io, path: list| -> list {
        let path = ext_val_as_str(n32, path.into_inner())?;
        let res = OpenOptions::new().create(true).write(true).truncate(true).open(path);
        let res = match res {
          Ok(f) => result_ok![n32; f: file],
          Err(e) => result_err![n32; e: error],
        };
        tuple![(): io, res: list]
      },
      "file_close" => |_io: io, _f: file| -> io {},
      "file_read_byte" => |_io: io, f: file| -> list {
        let mut buf = [0u8];
        let res = match f.read(&mut buf) {
          Ok(bytes) => {
            let byte = buf[0];
            let is_eof = bytes == 0;
            result_ok![n32; (tuple![byte: n32, is_eof: n32]): list]
          }
          Err(e) => result_err![n32; e: error],
        };
        tuple![(): io, f: file, res: list]
      },
      "file_read_char" => |_io: io, f: file| -> list {
        let res = match read_char(&*f) {
          Ok((_, Some(c))) => result_ok![n32; (tuple![c: n32, false: n32]): list],
          Ok((0, None)) => result_ok![n32; (tuple![0u32: n32, true: n32]): list],
          Ok(_) => result_ok![n32; (tuple![(char::REPLACEMENT_CHARACTER): n32, false: n32]): list],
          Err(e) => result_err![n32; e: error],
        };
        tuple![(): io, f: file, res: list]
      },
      "file_write_byte" => |_io: io, params: list| -> list {
        let (mut f, byte) = untuple![params; file, n32];
        let res = match f.write_all(&[byte as u8]) {
          Ok(()) => result_ok![n32; 1u32: n32],
          Err(e) => result_err![n32; e: error],
        };
        tuple![(): io, f: file, res: list]
      },
      "file_write_char" => |_io: io, params: list| -> list {
        let (mut f, c) = untuple![params; file, n32];
        let c = char::try_from(c).ok()?;
        let res = match write!(f, "{c}") {
          Ok(()) => result_ok![n32; 1u32: n32],
          Err(e) => result_err![n32; e: error],
        };
        tuple![(): io, f: file, res: list]
      },

      "io_error_code" => |e: error| -> (n32, error) {
        match e.kind() {
          ErrorKind::NotFound => (1, e),
          ErrorKind::PermissionDenied => (2, e),
          ErrorKind::AlreadyExists => (3, e),
          ErrorKind::NotADirectory => (4, e),
          ErrorKind::IsADirectory => (5, e),
          ErrorKind::ReadOnlyFilesystem => (6, e),
          _ => (0, e),
        }
      },
      "io_error_string" => |e: error| -> (list, error) {
        let error_string = format!("{:?}", &*e);
        let chars =
          error_string.chars().map(|c| n32.wrap_ext_val(c as u32)).collect::<Vec<ExtVal>>();
        (chars.into(), e)
      },
      "io_error_drop" => |e: error| {},

      "io_join" => |_io_a: io, _io_b: io| -> io {},
      "io_split" => |_io: io| -> (io, io) { ((), ()) },
      "io_ready" => |_io: io| -> (n32, io) { (1, ()) },

      "io_args" => |io0: io| -> (list, io) {
        let args = args
          .iter()
          .map(|s| {
            let chars = s.chars().map(|c| n32.wrap_ext_val(c as u32)).collect::<Vec<ExtVal>>();
            list.wrap_ext_val(chars.into())
          })
          .collect::<Vec<_>>()
          .into();
        (args, io0)
      },

      "io_print_char" => |_io: io, b: n32| -> io {
        write!(io_output_fn(), "{}", char::try_from(b).unwrap()).unwrap();
      },
      "io_print_byte" => |_io: io, b: n32| -> io {
        io_output_fn().write_all(&[b as u8]).unwrap();
      },
      "io_flush" => |_io: io| -> io {
        io_output_fn().flush().unwrap();
      },
      "io_read_byte" => |_io: io| -> (n32, io) {
        let mut buf = [0];
        let count = io_input_fn().read(&mut buf).unwrap();
        let byte = if count == 0 { u32::MAX } else { buf[0] as u32 };
        (byte, ())
      },
      "io_read_char" => |_io: io| -> (n32, io) {
        match read_char(io_input_fn()).ok()? {
          (0, None) => (u32::MAX, ()),
          (_, None) => (char::REPLACEMENT_CHARACTER as u32, ()),
          (_, Some(c)) => (c as u32, ()),
        }
      },
    });
  }
}

/// Attempts to read a single unicode character from `reader`, returning the
/// number of bytes read and whether the unicode character they correspond to.
/// If the read bytes are not a valid UTF-8 character, `(num_bytes, None)` is
/// returned.
fn read_char<R: Read>(mut reader: R) -> Result<(usize, Option<char>), io::Error> {
  let mut buf = [0];
  let count = reader.read(&mut buf)?;

  match (count, buf[0].leading_ones()) {
    (0, _) => Ok((0, None)),
    (_, 0) => Ok((1, Some(buf[0] as char))),
    (_, 2) => read_bytes_into_char(reader, buf[0], 1),
    (_, 3) => read_bytes_into_char(reader, buf[0], 2),
    (_, 4) => read_bytes_into_char(reader, buf[0], 3),
    _ => Ok((1, None)),
  }
}

/// Attempts to read `n` bytes from `reader`, and returns the actual number of
/// bytes read, and the `char` composed of the `first_byte` and the 1-3 newly
/// read bytes.
///
/// If less than `n` bytes are read, or if the bytes are invalid UTF-8,
/// `(num_bytes, None)` is returned.
fn read_bytes_into_char<R: Read>(
  mut reader: R,
  first_byte: u8,
  n: usize,
) -> Result<(usize, Option<char>), io::Error> {
  assert!((1..=3).contains(&n));

  let buf = &mut [0; 4][..(n + 1)];
  let count = reader.read(&mut buf[1..])?;
  if count != n {
    return Ok((count, None));
  }

  buf[0] = first_byte;

  Ok((n, str::from_utf8(buf).ok().and_then(|s| s.chars().next())))
}

fn ext_val_as_str<'ivm>(n32: ExtTy<'ivm, u32>, chars: Vec<ExtVal<'ivm>>) -> Option<String> {
  chars
    .into_iter()
    .map(|c| n32.unwrap_ext_val(c).map(char::try_from)?.ok())
    .collect::<Option<String>>()
}
