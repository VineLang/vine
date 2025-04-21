#[cfg(not(test))]
use core::sync::atomic::{AtomicUsize, Ordering};

#[cfg(test)]
use loom::sync::atomic::{AtomicUsize, Ordering};

use core::ops::Deref;

pub struct ArcInner<T: ?Sized> {
  strong: AtomicUsize,
  data: T,
}

impl<T> Deref for ArcInner<T> {
  type Target = T;
  fn deref(&self) -> &T {
    &self.data
  }
}

/// Custom reference-counted-value implementation
pub struct Arc<T: ?Sized> {
  inner: *const ArcInner<T>,
}

impl<T: ?Sized> Arc<T> {
  pub unsafe fn drop_with(self, f: unsafe fn(*mut ())) {
    if (*self.inner).strong.fetch_sub(1, Ordering::Release) != 1 {
      return;
    }
    f(&raw const (*self.inner).data as *mut ());
    drop(Box::from_raw(self.inner as *mut ()));
  }
}

impl<T> Arc<T> {
  pub fn new(data: T) -> Arc<T> {
    Self { inner: Box::into_raw(Box::new(ArcInner { strong: AtomicUsize::from(1), data })) }
  }
  pub fn from_raw(inner: *const ArcInner<T>) -> Self {
    Self { inner }
  }
  pub fn into_raw(self) -> *const ArcInner<T> {
    self.inner
  }
}
impl<T: ?Sized> Clone for Arc<T> {
  fn clone(&self) -> Self {
    unsafe {
      (*self.inner).strong.fetch_add(1, Ordering::Relaxed);
    }
    Self { inner: self.inner }
  }
}

unsafe impl<T: ?Sized + Send + Sync> Send for Arc<T> {}

#[cfg(test)]
mod tests {
  use super::*;
  use loom::thread;

  #[test]
  fn test_arc_thread_safety() {
    loom::model(|| {
      let mut dropped = 0;
      let arc = Arc::new(&raw mut dropped);

      let drop_it = |ptr| unsafe {
        let dropped = ptr as *mut *mut i32;
        (**dropped) += 1;
      };
      let mut handles = vec![];

      for _ in 0..3 {
        let arc_clone = arc.clone();
        handles.push(thread::spawn(move || unsafe {
          arc_clone.drop_with(drop_it);
        }));
      }

      unsafe {
        arc.drop_with(drop_it);
      }

      for handle in handles {
        handle.join().unwrap();
      }

      assert_eq!(dropped, 1);
    });
  }
}
