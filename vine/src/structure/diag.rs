use super::*;

use crate::structure::content::Content;

pub trait Diag<Ctx = ()> {}

pub struct Diagnostic {
  message: Content,
}
