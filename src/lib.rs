//!

// This is the "library" crate.
//
// It is conventional for proc macro crates to be wrapped up
// in a library crate, which reexport the proc macros, mostly because
// a proc macro crate cannot contain anything else.
//
// Currently our plans do not appear to include any items other than
// proc macros.  But we shouldn't foreclose that.  So for now
// this library crate ought to declare a dependency on the macro crate
// and reexport the macros.
//
// Also this crate is where the docs live, right here as crate-level
// docs.

pub use derive_adhoc_macros::{define_derive_adhoc, derive_adhoc, Adhoc};

#[doc(hidden)]
pub use derive_adhoc_macros::derive_adhoc_expand;
