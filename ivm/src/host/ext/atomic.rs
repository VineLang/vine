use std::ptr;

use ivy::name::Table;
use vine_util::register::Register;

use crate::{
  host::{
    Host,
    ext::{ExtFn, ExtTyRegister, Invalid, common::Nil},
  },
  runtime::{
    Runtime,
    addr::Addr,
    ext::ExtTyCast,
    port::Port,
    wire::Wire,
    word::{AtomicWord, Word},
  },
};

pub fn atomic<'ivm>() -> impl Register<Host<'ivm>> {
  (
    ExtFn("ivm:atomic:new", |host: &mut Host<'ivm>, _: &mut Table| {
      let token = host.register_ext_ty::<Token>();
      move |rt: &mut Runtime<'ivm, '_>, Nil, [out, value]: [Wire<'ivm>; 2]| {
        let (wire, _) = rt.new_wire();
        let atomic = unsafe { wire.addr().as_word() };
        let value = rt.follow(Port::new_wire(value));
        atomic.store(value.bits());
        let token = Port::new_ext_val(token.wrap(rt, Token(atomic, 0)));
        rt.link_wire(out, token);
      }
    }),
    ExtFn(
      "ivm:atomic:end",
      |rt: &mut Runtime<'ivm, '_>, Token(atomic, rank), [out]: [Wire<'ivm>; 1]| {
        if rank != 0 {
          rt.flags.ext_generic = true;
          rt.link_wire(out, Port::ERASE);
          return;
        }
        let port = unsafe { Port::from_bits(atomic.load()) };
        rt.free_wire(unsafe { Wire::from_addr(Addr(atomic as *const _ as *const ())) });
        rt.link_wire(out, port);
      },
    ),
    ExtFn(
      "ivm:atomic:token:split",
      |Token(atomic, rank)| -> Result<(Token<'ivm>, Token<'ivm>), Invalid> {
        let rank = rank.checked_add(1).ok_or(Invalid)?;
        Ok((Token(atomic, rank), Token(atomic, rank)))
      },
    ),
    ExtFn(
      "ivm:atomic:token:merge",
      |(a, b): (Token<'ivm>, Token<'ivm>)| -> Result<Token<'ivm>, Invalid> {
        if !ptr::eq(a.0, b.0) || a.1 != b.1 {
          return Err(Invalid);
        }
        Ok(Token(a.0, a.1 - 1))
      },
    ),
    ExtFn("ivm:atomic:token:access", |host: &mut Host<'ivm>, _: &mut Table| {
      let token = host.register_ext_ty::<Token>();
      move |rt: &mut Runtime<'ivm, '_>,
            Token(atomic, rank),
            [token_out, value, out]: [Wire<'ivm>; 3]| {
        let value = rt.follow(Port::new_wire(value));
        let value = unsafe { Port::from_bits(atomic.swap(value.bits())) };
        let token = token.wrap(rt, Token(atomic, rank));
        rt.link_wire(out, value);
        rt.link_wire(token_out, Port::new_ext_val(token));
      }
    }),
  )
}

struct Token<'ivm>(&'ivm AtomicWord, u64);

impl<'ivm> ExtTyRegister<'ivm> for Token<'ivm> {
  type With<'x> = Token<'x>;
}

impl<'ivm> ExtTyCast<'ivm> for Token<'ivm> {
  const COPY: bool = false;

  fn into_payload(rt: &mut Runtime<'ivm, '_>, Token(atomic, rank): Self) -> Word {
    Word::from_ptr(
      rt.make_pair(Word::from_ptr(atomic as *const _ as *const ()), Word::from_bits(rank)).0,
    )
  }

  unsafe fn from_payload(rt: &mut Runtime<'ivm, '_>, payload: Word) -> Self {
    let (atomic, rank) = unsafe { rt.take_pair(Addr(payload.ptr())) };
    Token(unsafe { &*(atomic.ptr() as *const AtomicWord) }, rank.bits())
  }
}
