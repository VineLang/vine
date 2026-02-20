use std::{
  fs::{File, OpenOptions},
  io::{self, ErrorKind, Read, Write},
  marker::PhantomData,
  mem::transmute,
};

use ivy::name::Table;
use vine_util::register::Register;

use crate::{
  host::{
    Host,
    ext::{
      ExtFn, ExtInput, ExtInputs, ExtOutput, ExtOutputs, ExtTyBoxed, ExtTyRegister, HostTable,
      Invalid, Live, error,
    },
  },
  runtime::{
    Runtime,
    addr::Addr,
    ext::{Boxed, ExtTyCast, ExtTyCastStatic, ExtVal},
    graft::Graft,
    port::Port,
    wire::Wire,
    word::Word,
  },
};

pub fn fundamental<'ivm, 'r>() -> impl Register<HostTable<'ivm, 'r>> {
  (
    ExtFn("ivm:pair", |(a, b): (ExtVal<'ivm>, ExtVal<'ivm>)| Pair(a, b)),
    ExtFn("ivm:unpair", |Pair(a, b)| (a, b)),
    ExtFn("ivm:ref", |host: &mut Host<'ivm>, _: &mut Table| {
      let ref_ = host.register_ext_ty();
      move |rt: &mut Runtime<'ivm, '_>, [a]: [ExtVal<'ivm>; 1], [b, wire]: [Wire<'ivm>; 2]| {
        let value = ref_.wrap(rt, Ref(a, b));
        rt.link_wire(wire, Port::new_ext_val(value));
      }
    }),
    ExtFn(
      "ivm:branch",
      |rt: &mut Runtime<'ivm, '_>,
       (index, branches): (u32, Branches<'ivm>),
       [wire]: [Wire<'ivm>; 1]| {
        let Some(branch) = branches.0.get(index as usize) else { return error(rt, [wire]) };
        let graft = Port::new_graft(unsafe { &**branch });
        rt.link_wire(wire, graft);
      },
    ),
    ExtFn("ivm:list:new", |Nil| Boxed::new(ExtList(Vec::new()))),
    ExtFn("ivm:list:len", |list: Boxed<ExtList<'ivm>>| (list.0.len() as u32, list)),
    ExtFn("ivm:list:push", |(mut list, value): (Boxed<ExtList<'ivm>>, ExtVal<'ivm>)| {
      list.0.push(value);
      list
    }),
    ExtFn("ivm:list:pop", |mut list: Boxed<ExtList<'ivm>>| {
      Ok((list.0.pop().ok_or(Invalid)?, list))
    }),
    ExtFn(
      "ivm:list:drop",
      |list: Boxed<ExtList<'ivm>>| {
        if list.0.is_empty() { Ok(()) } else { Err(Invalid) }
      },
    ),
  )
}

pub fn arithmetic<'ivm: 'r, 'r>() -> impl Register<HostTable<'ivm, 'r>> {
  use vine_util::arithmetic::{Define, arithmetic};

  struct Arithmetic<T>(PhantomData<T>);

  impl<
    'ivm,
    I: ExtInputs<'ivm, IN, IW>,
    const IN: usize,
    IW,
    O: ExtOutputs<'ivm, ON, OW>,
    const ON: usize,
    OW,
  > Define<I, O, Arithmetic<([IW; IN], [OW; ON])>> for HostTable<'ivm, '_>
  {
    fn define(&mut self, name: &'static str, f: impl 'static + Send + Sync + Fn(I) -> O) {
      self.host.register_ext_fn(self.table, name, f);
    }
  }

  arithmetic(Invalid)
}

pub fn io_meta<'ivm, 'r>() -> impl Register<HostTable<'ivm, 'r>> {
  (
    ExtFn("vi:io:split", |IO| (IO, IO)),
    ExtFn("vi:io:merge", |(IO, IO)| IO),
    ExtFn("vi:io:ready", |IO| (true, IO)),
  )
}

pub fn io_stdio<'ivm, 'r>() -> impl Register<HostTable<'ivm, 'r>> {
  io_stdio_with(io::stdin, io::stdout)
}

pub fn io_stdio_with<'ivm, 'r, R: Read, W: Write>(
  stdin: impl Copy + Fn() -> R + Send + Sync + 'ivm,
  stdout: impl Copy + Fn() -> W + Send + Sync + 'ivm,
) -> impl Register<HostTable<'ivm, 'r>> {
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

pub fn io_args<'ivm, 'r>(args: &'ivm [String]) -> impl Register<HostTable<'ivm, 'r>> {
  ExtFn("root:io:args", |IO| (List(args.iter().map(|str| List(str.chars()))), IO))
}

pub fn io_file<'ivm, 'r>() -> impl Register<HostTable<'ivm, 'r>> {
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

pub fn io_error<'ivm, 'r>() -> impl Register<HostTable<'ivm, 'r>> {
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

pub struct Nil;

impl<'ivm> ExtTyRegister<'ivm> for Nil {
  type With<'x> = Nil;
}

impl<'ivm> ExtTyCastStatic<'ivm> for Nil {
  const COPY: bool = true;

  #[inline(always)]
  fn into_payload_static(_: Nil) -> Word {
    Word::from_bits(0)
  }

  #[inline(always)]
  unsafe fn from_payload_static(_: Word) -> Nil {
    Nil
  }
}

pub struct Pair<'ivm>(pub ExtVal<'ivm>, pub ExtVal<'ivm>);

impl<'ivm> ExtTyRegister<'ivm> for Pair<'ivm> {
  type With<'x> = Pair<'x>;
}

impl<'ivm> ExtTyCast<'ivm> for Pair<'ivm> {
  const COPY: bool = false;

  fn into_payload(rt: &mut Runtime<'ivm, '_>, Pair(a, b): Self) -> Word {
    Word::from_ptr(rt.make_pair(a.bits(), b.bits()).0)
  }

  unsafe fn from_payload(rt: &mut Runtime<'ivm, '_>, payload: Word) -> Self {
    unsafe {
      let (a, b) = rt.take_pair(Addr(payload.ptr()));
      Pair(ExtVal::from_bits(a), ExtVal::from_bits(b))
    }
  }
}

pub struct Ref<'ivm>(pub ExtVal<'ivm>, pub Wire<'ivm>);

impl<'ivm> ExtTyRegister<'ivm> for Ref<'ivm> {
  type With<'x> = Ref<'x>;
}

impl<'ivm> ExtTyCast<'ivm> for Ref<'ivm> {
  const COPY: bool = false;

  fn into_payload(rt: &mut Runtime<'ivm, '_>, Ref(a, b): Self) -> Word {
    Word::from_ptr(rt.make_pair(a.bits(), Word::from_ptr(b.addr().0)).0)
  }

  unsafe fn from_payload(rt: &mut Runtime<'ivm, '_>, payload: Word) -> Self {
    unsafe {
      let (a, b) = rt.take_pair(Addr(payload.ptr()));
      Ref(ExtVal::from_bits(a), Wire::from_addr(Addr(b.ptr())))
    }
  }
}

impl<'ivm> ExtTyRegister<'ivm> for u32 {
  type With<'x> = Self;
}

impl<'ivm> ExtTyCastStatic<'ivm> for u32 {
  const COPY: bool = true;

  #[inline(always)]
  fn into_payload_static(value: u32) -> Word {
    Word::from_bits((value as u64) << 3)
  }

  #[inline(always)]
  unsafe fn from_payload_static(payload: Word) -> u32 {
    (payload.bits() >> 3) as u32
  }
}

impl<'ivm> ExtTyCast<'ivm> for u64 {
  const COPY: bool = true;

  #[inline(always)]
  fn into_payload(rt: &mut Runtime<'ivm, '_>, value: u64) -> Word {
    Word::from_ptr(rt.make_pair(Word::from_bits(value), Word::from_bits(0)).0)
  }

  #[inline(always)]
  unsafe fn from_payload(rt: &mut Runtime<'ivm, '_>, payload: Word) -> u64 {
    let (value, _) = unsafe { rt.take_pair(Addr(payload.ptr())) };
    value.bits()
  }
}

impl<'ivm> ExtTyRegister<'ivm> for f32 {
  type With<'x> = Self;
}

impl<'ivm> ExtTyCastStatic<'ivm> for f32 {
  const COPY: bool = true;

  #[inline(always)]
  fn into_payload_static(value: f32) -> Word {
    u32::into_payload_static(value.to_bits())
  }

  #[inline(always)]
  unsafe fn from_payload_static(payload: Word) -> f32 {
    f32::from_bits(unsafe { u32::from_payload_static(payload) })
  }
}

impl<'ivm> ExtTyRegister<'ivm> for f64 {
  type With<'x> = Self;
}

impl<'ivm> ExtTyCast<'ivm> for f64 {
  const COPY: bool = true;

  #[inline(always)]
  fn into_payload(rt: &mut Runtime<'ivm, '_>, value: f64) -> Word {
    u64::into_payload(rt, value.to_bits())
  }

  #[inline(always)]
  unsafe fn from_payload(rt: &mut Runtime<'ivm, '_>, payload: Word) -> f64 {
    f64::from_bits(unsafe { u64::from_payload(rt, payload) })
  }
}

impl<'ivm> ExtOutput<'ivm, ()> for bool {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let u32 = <u32 as ExtOutput<_>>::register(host, table);
    move |rt, value| u32(rt, value as u32)
  }
}

impl<'ivm> ExtInput<'ivm, ()> for i32 {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, ExtVal<'ivm>) -> Result<Self, Invalid>
  {
    let u32 = <u32 as ExtInput<_>>::register(host, table);
    move |rt, value| Ok(u32(rt, value)? as i32)
  }
}

impl<'ivm> ExtOutput<'ivm, ()> for i32 {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let u32 = <u32 as ExtOutput<_>>::register(host, table);
    move |rt, value| u32(rt, value as u32)
  }
}

impl<'ivm> ExtInput<'ivm, ()> for char {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, ExtVal<'ivm>) -> Result<Self, Invalid>
  {
    let u32 = <u32 as ExtInput<_>>::register(host, table);
    move |rt, value| char::from_u32(u32(rt, value)?).ok_or(Invalid)
  }
}

impl<'ivm> ExtOutput<'ivm, ()> for char {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let u32 = <u32 as ExtOutput<_>>::register(host, table);
    move |rt, value| u32(rt, value as u32)
  }
}

#[allow(clippy::borrowed_box)]
pub struct Branches<'ivm>(pub &'ivm Box<[*const Graft<'ivm>]>);

impl<'ivm> ExtTyRegister<'ivm> for Branches<'ivm> {
  type With<'x> = Branches<'x>;
}

impl<'ivm> ExtTyCastStatic<'ivm> for Branches<'ivm> {
  const COPY: bool = true;

  fn into_payload_static(branch: Branches<'ivm>) -> Word {
    Word::from_ptr(branch.0 as *const _ as *const ())
  }

  unsafe fn from_payload_static(payload: Word) -> Self {
    Branches(unsafe { &*(payload.ptr() as *const _) })
  }
}

pub struct ExtList<'ivm>(Vec<ExtVal<'ivm>>);

impl<'ivm> ExtTyBoxed<'ivm> for ExtList<'ivm> {
  type With<'x> = ExtList<'x>;
}

pub struct List<I>(I);
impl<'ivm, T: ExtOutput<'ivm, W>, W, I: IntoIterator<Item = T>> ExtOutput<'ivm, List<W>>
  for List<I>
{
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, T, I, W> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let convert_entry = T::register(host, table);
    let list = host.register_ext_ty();
    move |rt, List(iter)| {
      let vec = Vec::from_iter(iter.into_iter().map(|value| convert_entry(rt, value)));
      list.wrap(rt, Boxed::new(ExtList(vec)))
    }
  }
}

pub fn from_list<'ivm, T: ExtInput<'ivm, W>, W, C: FromIterator<T>>(
  host: &mut Host<'ivm>,
  table: &mut Table,
) -> impl use<'ivm, T, C, W> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, ExtVal<'ivm>) -> Result<C, Invalid>
{
  let input_entry = T::register(host, table);
  let list = host.register_ext_ty::<Boxed<ExtList<'ivm>>>();
  move |rt, value| {
    let vec = list.unwrap(rt, value).ok_or(Invalid)?.into_inner().0;
    vec.into_iter().map(|entry| input_entry(rt, entry)).collect()
  }
}

impl<'ivm, T: ExtInput<'ivm, W>, W> ExtInput<'ivm, Vec<W>> for Vec<T> {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, T, W>
  + Live<'ivm>
  + Fn(&mut Runtime<'ivm, '_>, ExtVal<'ivm>) -> Result<Self, Invalid> {
    from_list(host, table)
  }
}

impl<'ivm, T: ExtOutput<'ivm, W>, W> ExtOutput<'ivm, Vec<W>> for Vec<T> {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, T, W> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let handle = <List<_> as ExtOutput<_>>::register(host, table);
    move |rt, vec| handle(rt, List(vec))
  }
}

impl<'ivm> ExtInput<'ivm, ()> for String {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, ExtVal<'ivm>) -> Result<Self, Invalid>
  {
    from_list::<char, _, String>(host, table)
  }
}

impl<'ivm> ExtOutput<'ivm, ()> for String {
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let handle = <List<_> as ExtOutput<_>>::register(host, table);
    move |rt, str| unsafe { handle(rt, List(extend_lifetime(&str).chars())) }
  }
}

impl<'ivm, T: ExtOutput<'ivm, TW>, TW, E: ExtOutput<'ivm, EW>, EW> ExtOutput<'ivm, Result<TW, EW>>
  for Result<T, E>
{
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, T, E, TW, EW> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, Self) -> ExtVal<'ivm>
  {
    let handle_t = T::register(host, table);
    let handle_e = E::register(host, table);
    let n32 = host.register_ext_ty();
    let pair = host.register_ext_ty();
    move |rt, result| {
      let (tag, value) = match result {
        Err(err) => (n32.wrap_static(0), handle_e(rt, err)),
        Ok(value) => (n32.wrap_static(1), handle_t(rt, value)),
      };
      pair.wrap(rt, Pair(tag, value))
    }
  }
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

unsafe fn extend_lifetime<'a, T>(x: &T) -> &'a T {
  unsafe { transmute(x) }
}
