//! The Interaction Virtual Machine, an interaction combinator runtime.
//!
//! The [`IVM`] struct is the root of the API of this crate

#![cfg_attr(miri, feature(strict_provenance_lints))]
#![cfg_attr(miri, deny(fuzzy_provenance_casts, lossy_provenance_casts))]
#![warn(clippy::std_instead_of_core)]

mod ivm;
mod parallel;
pub use ivm::*;

pub mod addr;
pub mod ext;
pub mod global;
pub mod heap;
pub mod instruction;
pub mod port;
pub mod stats;
pub mod wire;

mod allocator;
mod interact;
mod word;
