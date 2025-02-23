use std::sync::atomic::{AtomicUsize, Ordering};

pub struct ArcInner<T: ?Sized> {
  strong: AtomicUsize,
  data: T,
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
    (*self.inner).strong.load(Ordering::Acquire);
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
