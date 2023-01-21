//! Tests for derive-adhoc
//!
//! Internal, unpublished crate.

#![cfg(test)]

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

mod list_names;
