//! Arrangements for testing derive-adhoc code directly
//!
//! We don't want to include all the exciting test code in
//! `derive-adhoc-macros`'s `cargo test`.
//!
//! Instead, we re-import the same source files here.
//!
//! Currently, this makes a testing version of derive-adhoc without any
//! of the optional features.

use macros::framework::*;
use macros::Concatenated;

use super::*;

use syn::parse_quote;

#[allow(dead_code)]
#[path = "../macros/macros.rs"]
pub(super) mod macros;

#[cfg(feature = "recent")] // examples may not work with old compiler
mod check_examples;
