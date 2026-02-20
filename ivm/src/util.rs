use std::cell::UnsafeCell;

pub struct Arena<T: ?Sized> {
  values: UnsafeCell<Vec<Option<Box<T>>>>,
}

impl<T: ?Sized> Default for Arena<T> {
  fn default() -> Self {
    Arena { values: Default::default() }
  }
}

impl<T: ?Sized> Arena<T> {
  #[allow(clippy::mut_from_ref)]
  pub fn push(&self, value: Box<T>) -> &mut T {
    unsafe {
      let ptr = Box::into_raw(value);
      let vec = &mut *self.values.get();
      let index = vec.len();
      vec.push(None);
      *(&mut vec[index] as *mut Option<Box<T>> as *mut *mut T) = ptr;
      &mut *ptr
    }
  }
}
