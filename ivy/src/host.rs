use std::{borrow::Cow, collections::HashMap};

use indexmap::{
  map::{raw_entry_v1::RawEntryMut, RawEntryApiV1},
  IndexMap,
};

use ivm::global::Global;

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

  /// This is an `IndexMap` instead of an `IndexSet` so that the raw entry API
  /// can be used.
  labels: IndexMap<String, ()>,
}

impl<'ivm> Host<'ivm> {
  pub fn get(&self, name: &str) -> Option<&'ivm Global<'ivm>> {
    Some(unsafe { &**self.globals.get(name)? })
  }

  fn get_raw(&self, name: &str) -> Option<*const Global<'ivm>> {
    self.globals.get(name).copied()
  }

  pub fn label_to_u16<'l>(&mut self, label: impl Into<Cow<'l, str>>) -> u16 {
    let label = label.into();
    match self.labels.raw_entry_mut_v1().from_key(&*label) {
      RawEntryMut::Occupied(e) => e.index() as u16,
      RawEntryMut::Vacant(e) => {
        e.insert(label.into(), ());
        (self.labels.len() - 1) as u16
      }
    }
  }

  pub fn label_from_u16(&self, label: u16) -> &str {
    self.labels.get_index(label as usize).unwrap().0
  }
}
