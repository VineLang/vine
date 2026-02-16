use std::{borrow::Cow, cell::UnsafeCell, collections::HashMap};

use indexmap::{
  IndexMap,
  map::{RawEntryApiV1, raw_entry_v1::RawEntryMut},
};

use ivm::{
  addr::Addr,
  ext::{Ext, ExtFn, ExtTy, ExtTyId, ExtVal, IO, Lookup},
  global::Global,
  port::{Port, Tag},
};

use crate::ast::Tree;

mod ext;
mod readback;
mod serialize;

#[derive(Default)]
pub struct Host<'ivm> {
  /// Safety invariant: until dropping, the outer vec is only ever pushed to;
  /// particularly, the contents of the inner vecs are never accessed from here
  /// (though they may be accessed via the pointers stored in `self.globals`).
  global_store: Vec<Vec<Global<'ivm>>>,

  /// For most purposes, this could be a `HashMap<String, &'ivm Global<'ivm>>`,
  /// except that in [`serialize`], we need a few of those pointers to
  /// temporarily be `&UnsafeCell<Global<'ivm>>`s.
  globals: HashMap<String, *const Global<'ivm>>,

  /// Lookup tables of trivially-copyable nilary ports. These are currently only
  /// used for and constructible through branch nodes.
  #[allow(clippy::vec_box)]
  lookups: Vec<Box<Vec<Port<'ivm>>>>,

  ext_fns: HashMap<String, ExtFn<'ivm>>,
  reverse_ext_fns: HashMap<ExtFn<'ivm>, String>,

  ext_tys: HashMap<String, ExtTyId<'ivm>>,
  reverse_ext_tys: HashMap<ExtTyId<'ivm>, String>,

  /// This is an `IndexMap` instead of an `IndexSet` so that the raw entry API
  /// can be used.
  pub comb_labels: IndexMap<String, ()>,
  pub opaque_ext_fn_labels: IndexMap<String, ()>,
}

impl<'ivm> Host<'ivm> {
  pub fn get_ref(&self, name: &str) -> Option<&'ivm Global<'ivm>> {
    Some(unsafe { &**self.globals.get(name)? })
  }

  pub(crate) fn get_port(&self, name: &str) -> Port<'ivm> {
    let global = self.globals.get(name).copied().expect("undefined global");
    // Safety: upholds the requirements of `Tag::Global`, and preserves the interior
    // mutability.
    unsafe { Port::new(Tag::Global, 0, Addr(global as *const UnsafeCell<Global> as *const ())) }
  }

  pub fn label_to_u16<'l>(
    label: impl Into<Cow<'l, str>>,
    labels: &mut IndexMap<String, ()>,
  ) -> u16 {
    let label = label.into();
    match labels.raw_entry_mut_v1().from_key(&*label) {
      RawEntryMut::Occupied(e) => e.index() as u16,
      RawEntryMut::Vacant(e) => {
        e.insert(label.into(), ());
        (labels.len() - 1) as u16
      }
    }
  }

  pub fn label_from_u16(label: u16, labels: &IndexMap<String, ()>) -> &str {
    labels.get_index(label as usize).unwrap().0
  }

  fn ext_ty<T: Ext<'ivm>>(&self) -> ExtTy<'ivm, T> {
    ExtTy::new_unchecked(self.ext_tys[T::NAME])
  }

  pub fn new_f32(&self, payload: f32) -> ExtVal<'ivm> {
    self.ext_ty::<f32>().wrap_ext_val(payload)
  }

  pub fn new_f64(&self, payload: f64) -> ExtVal<'ivm> {
    self.ext_ty::<f64>().wrap_ext_val(payload)
  }

  pub fn new_n32(&self, payload: u32) -> ExtVal<'ivm> {
    self.ext_ty::<u32>().wrap_ext_val(payload)
  }

  pub fn new_io(&self) -> ExtVal<'ivm> {
    self.ext_ty::<IO>().wrap_ext_val(IO)
  }

  pub fn new_lookup(&mut self, lookup: &[Tree]) -> Option<Port<'ivm>> {
    fn copy_into_port<'ivm>(host: &mut Host<'ivm>, tree: &Tree) -> Option<Port<'ivm>> {
      match tree {
        Tree::Erase => Some(Port::ERASE),
        Tree::N32(n) => Some(Port::new_ext_val(host.new_n32(*n))),
        Tree::F32(f) => Some(Port::new_ext_val(host.new_f32(*f))),
        Tree::Lookup(bs) => host.new_lookup(bs),
        Tree::Global(g) => Some(host.get_port(g)),
        Tree::F64(_) | Tree::Comb(..) | Tree::ExtFn(..) | Tree::Branch(..) | Tree::Var(_) => None,
        Tree::BlackBox(t) => copy_into_port(host, t),
      }
    }

    let ports: Vec<Port<'ivm>> =
      lookup.iter().map(|tree| copy_into_port(self, tree)).collect::<Option<_>>()?;

    let lookup = Box::new(ports);
    self.lookups.push(lookup);
    let ptr = &raw const **self.lookups.last().unwrap();
    let ext = self.ext_ty::<Lookup<'ivm>>().wrap_ext_val(Lookup(ptr));

    Some(Port::new_ext_val(ext))
  }

  pub fn instantiate_ext_fn(&self, ext_fn_name: &str, swap: bool) -> Option<ExtFn<'ivm>> {
    let mut ext_fn = *self.ext_fns.get(ext_fn_name)?;
    if swap {
      ext_fn = ext_fn.swapped()
    }
    Some(ext_fn)
  }
}
