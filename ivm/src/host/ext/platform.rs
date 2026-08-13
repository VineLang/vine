use alloc::string::String;
use std::{
  format,
  fs::{File, OpenOptions},
  io::{self, ErrorKind, Read, Write},
};

use vine_util::register::Register;

use crate::{
  host::{
    Host,
    ext::{
      ExtFn, ExtTyBoxed, ExtTyRegister, Invalid,
      common::{List, Nil},
    },
  },
  runtime::{
    ext::{Boxed, ExtTyCastStatic},
    word::Word,
  },
};

pub fn all<'ivm, R: Read, W: Write>(
  args: &'ivm [String],
  stdin: impl Copy + Fn() -> R + Send + Sync + 'ivm,
  stdout: impl Copy + Fn() -> W + Send + Sync + 'ivm,
) -> impl Register<Host<'ivm>> {
  (io_meta(), io_args(args), io_stdio_with(stdin, stdout), io_error(), io_file())
}

pub fn io_meta<'ivm>() -> impl Register<Host<'ivm>> {
  (
    ExtFn("vi:io:split", |IO| (IO, IO)),
    ExtFn("vi:io:merge", |(IO, IO)| IO),
    ExtFn("vi:io:ready", |IO| (true, IO)),
  )
}

pub fn io_args<'ivm>(args: &'ivm [String]) -> impl Register<Host<'ivm>> {
  ExtFn("root:io:args", |IO| (List(args.iter().map(|str| List(str.chars()))), IO))
}

pub struct IO;

impl<'ivm> ExtTyRegister<'ivm> for IO {
  type With<'x> = IO;
}

impl<'ivm> ExtTyCastStatic<'ivm> for IO {
  const COPY: bool = false;

  fn into_payload_static(_: IO) -> Word {
    Word::from_bits(0)
  }

  unsafe fn from_payload_static(_: Word) -> Self {
    IO
  }
}

pub fn io_stdio<'ivm>() -> impl Register<Host<'ivm>> {
  io_stdio_with(io::stdin, io::stdout)
}

pub fn io_stdio_with<'ivm, R: Read, W: Write>(
  stdin: impl Copy + Fn() -> R + Send + Sync + 'ivm,
  stdout: impl Copy + Fn() -> W + Send + Sync + 'ivm,
) -> impl Register<Host<'ivm>> {
  (
    ExtFn("root:io:print_char", move |(IO, n): (IO, u32)| -> IO {
      write!(stdout(), "{}", char::try_from(n).unwrap()).unwrap();
      IO
    }),
    ExtFn("root:io:print_byte", move |(IO, b): (IO, u32)| -> IO {
      stdout().write_all(&[b as u8]).unwrap();
      IO
    }),
    ExtFn("root:io:flush", move |IO| -> IO {
      stdout().flush().unwrap();
      IO
    }),
    ExtFn("root:io:read_byte", move |IO| -> (u32, IO) {
      let mut buf = [0];
      let count = stdin().read(&mut buf).unwrap();
      let byte = if count == 0 { u32::MAX } else { buf[0] as u32 };
      (byte, IO)
    }),
    ExtFn("root:io:read_char", move |IO| -> Result<(u32, IO), Invalid> {
      Ok(match read_char(stdin()).map_err(|_| Invalid)? {
        (0, None) => (u32::MAX, IO),
        (_, None) => (char::REPLACEMENT_CHARACTER as u32, IO),
        (_, Some(c)) => (c as u32, IO),
      })
    }),
  )
}

pub fn io_file<'ivm>() -> impl Register<Host<'ivm>> {
  (
    ExtFn("root:io:file:open", |(_, path): (IO, String)| {
      (OpenOptions::new().read(true).append(true).open(path), IO)
    }),
    ExtFn("root:io:file:create", |(_, path): (IO, String)| {
      (OpenOptions::new().create(true).write(true).truncate(true).open(path), IO)
    }),
    ExtFn("root:io:file:close", |_: (IO, File)| IO),
    ExtFn("root:io:file:read_byte", |(_, mut file): (IO, Boxed<File>)| {
      let mut buf = [0u8];
      (file.read(&mut buf).map(|read| if read == 0 { u32::MAX } else { buf[0] as u32 }), IO, file)
    }),
    ExtFn("root:io:file:read_char", |(_, file): (IO, Boxed<File>)| {
      let result = match read_char(&*file) {
        Ok((_, Some(char))) => Ok(char as u32),
        Ok((0, None)) => Ok(u32::MAX),
        Ok(_) => Ok(char::REPLACEMENT_CHARACTER as u32),
        Err(e) => Err(e),
      };
      (result, IO, file)
    }),
    ExtFn("root:io:file:write_byte", |(_, mut file, byte): (IO, Boxed<File>, u32)| {
      (file.write_all(&[byte as u8]).map(|_| Nil), IO, file)
    }),
    ExtFn("root:io:file:write_char", |(_, mut file, char): (IO, Boxed<File>, char)| {
      (write!(file, "{char}").map(|_| Nil), IO, file)
    }),
  )
}

pub fn io_error<'ivm>() -> impl Register<Host<'ivm>> {
  (
    ExtFn("root:io:error:code", |error: Boxed<io::Error>| {
      let code = match error.kind() {
        ErrorKind::NotFound => 1,
        ErrorKind::PermissionDenied => 2,
        ErrorKind::AlreadyExists => 3,
        ErrorKind::NotADirectory => 4,
        ErrorKind::IsADirectory => 5,
        ErrorKind::ReadOnlyFilesystem => 6,
        _ => 0,
      };
      (code, error)
    }),
    ExtFn("root:io:error:message", |error: Boxed<io::Error>| {
      let message = format!("{:?}", &*error);
      (message, error)
    }),
    ExtFn("root:io:error:drop", |_: io::Error| {}),
  )
}

impl<'ivm> ExtTyBoxed<'ivm> for File {
  type With<'x> = File;
}

impl<'ivm> ExtTyBoxed<'ivm> for io::Error {
  type With<'x> = io::Error;
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
