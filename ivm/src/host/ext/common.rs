use alloc::{boxed::Box, string::String, vec::Vec};
use core::{marker::PhantomData, mem::transmute};

use ivy::name::Table;
use vine_util::register::Register;

use crate::{
  host::{
    Host,
    ext::{
      ExtFn, ExtInput, ExtInputs, ExtOutput, ExtOutputs, ExtTyBoxed, ExtTyRegister, Invalid, Live,
      error,
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

pub fn all<'ivm>() -> impl Register<Host<'ivm>> {
  (fundamental(), arithmetic())
}

pub fn fundamental<'ivm>() -> impl Register<Host<'ivm>> {
  (
    ExtFn("ivm:pair", |(a, b): (ExtVal<'ivm>, ExtVal<'ivm>)| Pair(a, b)),
    ExtFn("ivm:unpair", |Pair(a, b)| (a, b)),
    ExtFn("ivm:ref", |host: &mut Host<'ivm>, _: &mut Table| {
      let ref_ = host.register_ext_ty::<Ref>();
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

pub fn arithmetic<'ivm>() -> impl Register<Host<'ivm>> {
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
  > Define<I, O, Arithmetic<([IW; IN], [OW; ON])>> for Host<'ivm>
  {
    fn define(
      (host, table): (&mut Host<'ivm>, &mut Table),
      name: &'static str,
      f: impl 'static + Send + Sync + Fn(I) -> O,
    ) {
      host.register_ext_fn(table, name, f);
    }
  }

  arithmetic(Invalid)
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
  const COPY: bool = false;

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
  const COPY: bool = false;

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

// We need to use a borrowed box here since a `&[T]` would be a fat pointer
// (ptr + size) which wouldn't fit into a 64-bit `Word`.
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

pub struct List<I>(pub(super) I);
impl<'ivm, T: ExtOutput<'ivm, W>, W, I: IntoIterator<Item = T>> ExtOutput<'ivm, List<W>>
  for List<I>
{
  fn register(
    host: &mut Host<'ivm>,
    table: &mut Table,
  ) -> impl use<'ivm, T, I, W> + Live<'ivm> + Fn(&mut Runtime<'ivm, '_>, Self) -> ExtVal<'ivm> {
    let convert_entry = T::register(host, table);
    let list = host.register_ext_ty::<Boxed<ExtList>>();
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
    // SAFETY: `handle` drains the `Chars` iterator synchronously before returning,
    // and `str` is owned by this call, so the borrow never actually escapes past
    // the local stack frame despite its forged `'ivm` lifetime.
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
    let n32 = host.register_ext_ty::<u32>();
    let pair = host.register_ext_ty::<Pair>();
    move |rt, result| {
      let (tag, value) = match result {
        Err(err) => (n32.wrap_static(0), handle_e(rt, err)),
        Ok(value) => (n32.wrap_static(1), handle_t(rt, value)),
      };
      pair.wrap(rt, Pair(tag, value))
    }
  }
}

unsafe fn extend_lifetime<'a, T>(x: &T) -> &'a T {
  unsafe { transmute(x) }
}
