use crate::structures::content::Content;

pub trait Diag<Ctx = ()> {}

pub struct Diagnostic {
  message: Content,
}
