#![cfg_attr(miri, feature(strict_provenance_lints))]
#![cfg_attr(miri, deny(fuzzy_provenance_casts, lossy_provenance_casts))]
#![no_std]

extern crate alloc;

pub mod host;
pub mod program;
pub mod runtime;
pub mod system;
mod util;
