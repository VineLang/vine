use std::{
  fs::File,
  io::{self, Error, Read, Write},
  ops::{Add, Div, Mul, Sub},
  path::PathBuf,
};

use ivm::{
  arc::{Arc, ArcInner},
  ext::{ExtFn, ExtTy, ExtVal, Extrinsics},
};

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
    let f32 = extrinsics.register_unboxed_ext_ty();
    let io = extrinsics.register_unboxed_ext_ty();
    let ioerr = extrinsics.register_rc_ext_ty(|ptr| unsafe {
      let err = ptr as *mut *mut Error;
      drop(Box::from_raw(*err));
    });
    let file = extrinsics.register_rc_ext_ty(|ptr| unsafe {
      let file = ptr as *mut *mut File;
      drop(Box::from_raw(*file));
    });
    let pathbuf = extrinsics.register_rc_ext_ty(|ptr| unsafe {
      let path = ptr as *mut *mut PathBuf;
      drop(Box::from_raw(*path));
    });

    self.register_ext_ty("N32".into(), n32);
    self.register_ext_ty("F32".into(), f32);
    self.register_ext_ty("IO".into(), io);
    self.register_ext_ty("IOError".into(), ioerr);
    self.register_ext_ty("File".into(), file);
    self.register_ext_ty("PathBuf".into(), pathbuf);

    let as_n32 = move |x: ExtVal<'ivm>| x.as_unboxed_ty(&n32);
    let as_f32 = move |x: ExtVal<'ivm>| f32::from_bits(x.as_unboxed_ty(&f32));
    let new_n32 = move |x: u32| ExtVal::new_unboxed(n32, x);
    let new_n32 = move |x: u32| ExtVal::new_unboxed(n32, x);
    let new_f32 = move |x: f32| ExtVal::new_unboxed(f32, x.to_bits());
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
      "f32_to_n32" => |a, _b| new_n32(as_f32(a) as u32),
      "f32_to_bits" => |a, _b| new_n32(as_f32(a).to_bits()),
      "f32_from_bits" => |a, _b| new_f32(f32::from_bits(as_n32(a))),

      "i32_div" => |a, b| new_n32((as_n32(a) as i32 / as_n32(b) as i32) as u32),
      "i32_rem" => |a, b| new_n32((as_n32(a) as i32 % as_n32(b) as i32) as u32),
      "i32_shr" => |a, b| new_n32((as_n32(a) as i32).wrapping_shr(as_n32(b)) as u32),
      "i32_lt" => |a, b| new_bool((as_n32(a) as i32) < (as_n32(b) as i32)),
      "i32_le" => |a, b| new_bool((as_n32(a) as i32) <= (as_n32(b) as i32)),

      // "n32_add" => |a, b| new_n32(as_n32(a).wrapping_add(as_n32(b))),
      // "n32_sub" => |a, b| new_n32(as_n32(a).wrapping_sub(as_n32(b))),
      // "n32_mul" => |a, b| new_n32(as_n32(a).wrapping_mul(as_n32(b))),
      // "n32_div" => |a, b| new_n32(as_n32(a).wrapping_div(as_n32(b))),
      // "n32_rem" => |a, b| new_n32(as_n32(a).wrapping_rem(as_n32(b))),

      // "eq" => |a, b| comparison(n32, f32, a, b, |a, b| a == b, |a, b| a == b),
      // "ne" => |a, b| comparison(n32, f32, a, b, |a, b| a != b, |a, b| a != b),
      // "lt" => |a, b| comparison(n32, f32, a, b, |a, b| a < b, |a, b| a < b),
      // "le" => |a, b| comparison(n32, f32, a, b, |a, b| a <= b, |a, b| a <= b),

      // "n32_shl" => |a, b| ExtVal::new_unboxed(n32, a.as_unboxed_ty(&n32).wrapping_shl(b.as_unboxed_ty(&n32))),
      // "n32_shr" => |a, b| ExtVal::new_unboxed(n32, a.as_unboxed_ty(&n32).wrapping_shr(b.as_unboxed_ty(&n32))),
      // "n32_rotl" => |a, b| ExtVal::new_unboxed(n32, a.as_unboxed_ty(&n32).rotate_left(b.as_unboxed_ty(&n32))),
      // "n32_rotr" => |a, b| ExtVal::new_unboxed(n32, a.as_unboxed_ty(&n32).rotate_right(b.as_unboxed_ty(&n32))),

      // "n32_and" => |a, b| ExtVal::new_unboxed(n32, a.as_unboxed_ty(&n32) & b.as_unboxed_ty(&n32)),
      // "n32_or" => |a, b| ExtVal::new_unboxed(n32, a.as_unboxed_ty(&n32) | b.as_unboxed_ty(&n32)),
      // "n32_xor" => |a, b| ExtVal::new_unboxed(n32, a.as_unboxed_ty(&n32) ^ b.as_unboxed_ty(&n32)),

      // "n32_add_high" => |a, b| ExtVal::new_unboxed(n32, (((a.as_unboxed_ty(&n32) as u64) + (b.as_unboxed_ty(&n32) as u64)) >> 32) as u32),
      // "n32_mul_high" => |a, b| ExtVal::new_unboxed(n32, (((a.as_unboxed_ty(&n32) as u64) * (b.as_unboxed_ty(&n32) as u64)) >> 32) as u32),

      "io_print_char" => |a, b| {
        a.as_unboxed_ty(&io);
        print!("{}", char::try_from(b.as_unboxed_ty(&n32)).unwrap());
        ExtVal::new_unboxed(io, 0)
      },
      "io_print_byte" => |a, b| {
        a.as_unboxed_ty(&io);
        io::stdout().write_all(&[b.as_unboxed_ty(&n32) as u8]).unwrap();
        ExtVal::new_unboxed(io, 0)
      },
      "io_flush" => |a, _b| {
        a.as_unboxed_ty(&io);
        io::stdout().flush().unwrap();
        ExtVal::new_unboxed(io, 0)
      },
      "io_read_byte" => |a, b| {
        a.as_unboxed_ty(&io);
        let default = b.as_unboxed_ty(&n32) as u8;
        let mut buf = [default];
        _ = io::stdin().read(&mut buf).unwrap();
        ExtVal::new_unboxed(n32, buf[0] as u32)
      },

      "new_path" => |a, _b| {
        a.as_unboxed_ty(&io);
        unsafe {
          ExtVal::new_rc_raw(pathbuf, Arc::into_raw(Arc::new(
            Box::into_raw(Box::new(PathBuf::new()))
          )) as _)
        }
      },

      "path_push" => |a, b| unsafe {
        let path_inner = a.as_rc_ty(&pathbuf) as *const ArcInner<*mut PathBuf>;
        let c = char::try_from(b.as_unboxed_ty(&n32)).unwrap();
        (***path_inner).push(c.to_string());
        ExtVal::new_rc_raw(pathbuf, path_inner as _)
      },

      "file_open" => |a, b| unsafe {
        a.as_unboxed_ty(&io);
        let path_inner = b.as_rc_ty(&pathbuf) as *const ArcInner<*mut PathBuf>;
        match File::open(&***path_inner) {
            Err(reason) => ExtVal::new_rc_raw(ioerr, Arc::into_raw(Arc::new(
              Box::into_raw(Box::new(reason))
            )) as _),
            Ok(handle) => ExtVal::new_rc_raw(file, Arc::into_raw(Arc::new(
              Box::into_raw(Box::new(handle))
            )) as _),
        }
      },

      "is_error" => |a, _b| {
        ExtVal::new_unboxed(n32, if a.ty() == ioerr { 1 } else { 0 })
      },

      "io_error_code" => |a, _b| unsafe {
        let err_inner = a.as_rc_ty(&ioerr) as *const ArcInner<*mut Error>;
        ExtVal::new_unboxed(n32, match (***err_inner).raw_os_error() {
          Some(err_code) => err_code.try_into().unwrap(),
          None => u32::MAX
        })
      },

      "file_read_byte" => |a, b| unsafe {
        let file_inner = a.as_rc_ty(&file) as *const ArcInner<*mut File>;
        b.as_unboxed_ty(&n32);
        let mut buf = [0];
        match (***file_inner).read(&mut buf) {
          Ok(bytes_read) => if bytes_read > 0 {
            ExtVal::new_unboxed(n32, buf[0] as u32)
          } else {
            b
          },
          Err(reason) => ExtVal::new_rc_raw(ioerr, Arc::into_raw(Arc::new(
            Box::into_raw(Box::new(reason))
          )) as _)
        }
      },

      "file_write_byte" => |a, b| unsafe {
        let file_inner = a.as_rc_ty(&file) as *const ArcInner<*mut File>;
        match (***file_inner).write_all(&[b.as_unboxed_ty(&n32) as u8]) {
          Ok(()) => ExtVal::new_unboxed(n32, 0),
          Err(reason) => ExtVal::new_rc_raw(ioerr, Arc::into_raw(Arc::new(
            Box::into_raw(Box::new(reason))
          )) as _)
        }
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
  }
}
