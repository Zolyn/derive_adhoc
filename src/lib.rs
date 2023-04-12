#![doc=include_str!("../README.md")]

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

pub use derive_adhoc_macros::derive_adhoc_expand;
pub use derive_adhoc_macros::{define_derive_adhoc, derive_adhoc, Adhoc};

// We (ab)use the module system as places to hang our documentation.

#[doc=include_str!("../doc/introduction.md")]
pub mod doc_introduction {}

#[doc=include_str!("../doc/reference.md")]
pub mod doc_reference {}

#[doc=include_str!("../doc/implementation.md")]
pub mod doc_implementation {}

#[doc=include_str!("../CHANGELOG.md")]
pub mod doc_changelog {}

#[cfg(not(feature = "minimal-1"))]
compile_error! { "You must enable at least one derive-adhoc crate feature!" }
