use std::{borrow::Cow, collections::HashMap};

use indexmap::{
  IndexMap,
  map::{RawEntryApiV1, raw_entry_v1::RawEntryMut},
};

use ivm::{
  ext::{ExtFn, ExtTy, ExtVal},
  global::Global,
};

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

  ext_fns: HashMap<String, ExtFn<'ivm>>,
  reverse_ext_fns: HashMap<ExtFn<'ivm>, String>,

  ext_tys: HashMap<String, ExtTy<'ivm>>,
  reverse_ext_tys: HashMap<ExtTy<'ivm>, String>,

  /// This is an `IndexMap` instead of an `IndexSet` so that the raw entry API
  /// can be used.
  pub comb_labels: IndexMap<String, ()>,
  pub opaque_ext_fn_labels: IndexMap<String, ()>,
}

impl<'ivm> Host<'ivm> {
  pub fn get(&self, name: &str) -> Option<&'ivm Global<'ivm>> {
    Some(unsafe { &**self.globals.get(name)? })
  }

  fn get_raw(&self, name: &str) -> Option<*const Global<'ivm>> {
    self.globals.get(name).copied()
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

  pub fn new_f32(&self, payload: f32) -> ExtVal<'ivm> {
    ExtVal::new(self.ext_tys["F32"], payload.to_bits() as u64)
  }

  pub fn new_n32(&self, payload: u32) -> ExtVal<'ivm> {
    ExtVal::new(self.ext_tys["N32"], payload as u64)
  }

  pub fn new_io(&self) -> ExtVal<'ivm> {
    ExtVal::new(self.ext_tys["IO"], 0)
  }

  pub fn instantiate_ext_fn(&self, ext_fn_name: &str, swap: bool) -> Option<ExtFn<'ivm>> {
    let mut ext_fn = *self.ext_fns.get(ext_fn_name)?;
    if swap {
      ext_fn = ext_fn.swapped()
    }
    Some(ext_fn)
  }
}
