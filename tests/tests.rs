//! Tests for derive-adhoc
//!
//! Internal, unpublished crate.

use std::fmt::Debug;

use easy_ext::ext;

// This package has the following tests:
//
//  expand/*.rs
//    run with trybuild, and expected to pass
//    expanded with macrotest, compared with expand/*.expanded.rs
//
//  ui/*.rs
//    run with trybuild and expected to fail
//    errors compared with ui/*.stderr
//
// The test modules listed here, containing #[test] tests:

#[cfg(test)]
mod list_names;

#[cfg(test)]
mod modules;

#[ext(DebugExt)]
pub impl<T: Debug> T {
    fn to_debug(&self) -> String {
        format!("{:?}", self)
    }
}
