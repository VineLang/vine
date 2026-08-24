#![cfg_attr(miri, feature(strict_provenance_lints))]
#![cfg_attr(miri, deny(fuzzy_provenance_casts, lossy_provenance_casts))]
#![no_std]

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

pub mod host;
#[cfg(feature = "std")]
pub mod program;
pub mod runtime;
pub mod system;
mod util;
