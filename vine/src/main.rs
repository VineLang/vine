pub mod component;
pub mod feature;
pub mod structure;

pub(crate) use macro_rules_attribute::derive as derive_;

pub(crate) use derive_more::From;

pub(crate) use vine_util::{
  for_enum,
  idx::{Idx, IdxVec},
  new_idx,
  visit::Visit,
};

pub(crate) use std::{
  fmt::{self, Debug, Display},
  mem::{replace, take},
};

pub(crate) use crate::structure::ast::*;

fn main() {
  println!("Hello, world!");
}
