//! Tests for derive-adhoc
//!
//! **Internal, unpublished crate.**
//!
//! Separating the tests into their own crate ensures that they
//! all go through the "front door" of `use derive_adhoc::...`,
//! and in particular makes the cross-crate tests uniform with the others.
//!
//! ## Invoking the tests
//!
//! When running the full test suite,
//! you should use a particular version of Nightly Rust:
//! ```text
//! rustup toolchain add nightly-2022-11-12
//! cargo +nightly-2022-11-12 test --locked --workspace --all-features
//! ```
//! With other versions, the pretty printing of the macro expansions,
//! or the results of standard library macros, can change,
//! causing the tests to break.
//!
//! After your first run, it will probably be helpful to say
//! ```text
//! CARGO_NET_OFFLINE=true cargo +nightly-2022-11-12 test --locked --workspace --all-features
//!```
//! This is because otherwise cargo
//! will uselessly re-update the cargo index,
//! when it is reinvoked by some of the tests.
//!
//! ## Updating the expected outputs
//!
//! Many of the test cases compare an actual with expected output.
//! For example, an expected macro expansion, or error message.
//!
//! If this is expected
//! (for example, you added to the tests, or fixed a bug)
//! can update the expected output files from the actual output.
//!
//! ```text
//! TRYBUILD=overwrite MACROTEST=overwrite STDERRTEST=overwrite \
//! CARGO_NET_OFFLINE=true cargo +nightly-2022-11-12 test --locked --workspace --all-features
//! ```
//!
//! *Check that the actual output is as desired!*
//! This is easily done with `git diff` before committing.
//!
//! ## Classes of test in the derive-adhoc workspace
//!
//! Testing proc macros is not entirely straightforward,
//! so there are multiple classes of test with different approaches.
//!
//! ## Normal tests directly in this module
//!
//! Listed below, with `mod` lines referring to `.rs` files
//! which contain `#[test]` functions.
//!
//! ### `tests/expand/*.rs`
//!
//!  - Run with [`trybuild`], and expected to pass.
//!  - Expanded with `macrotest`, compared with `expand/*.expanded.rs`.
//!
//! Invoked from `tests/macrotest.rs`,
//! using [`list_expand_test_paths`]
//! and [`list_expand_test_paths_for_macrotest`].
//!
//! Tests with `recent` somewhere in their name are only run
//! if the `recent` cargo feature is enabled for `derive-adhoc-tests`.
//! This is enabled in CI for the newer compilers, but not for the MSRV.
//!
//! ### `tests/ui/*.rs`
//!
//!  - Run with trybuild and expected to fail.
//!  - Errors compared with `tests/ui/*.stderr`.
//!
//! Invoked from `tests/trybuild.rs`.
//!
//! Some of these `.rs` files are reused for `tests/stderr/`.
//!
//! ### `tests/stderr/`
//!
//! Test cases listed in `tests/stderr/stderr-lib.rs`.
//!
//!  - Compile attempted, and stderr captured
//!  - Output compared with `tests/stderr/combined.real-stderr`
//!
//! Invoked from `tests/stderr.rs`,
//! which builds the crate `derive-adhoc-stderr-tests`.
//! See the doc comment there for details and rationale.
//!
//! ### `tests/pub-a/`, `tests/pub-b/`
//!
//! Tests of cross-crate exports of macros and templates.
//! `pub-a` exports things, and `pub-b` imports and uses them.
//!
//! `pub-a` uses a special `bizarre` version of derive-adhoc,
//! to test that the right template expander is used in each case.
//! See the [`pub-b` doc comment](../pub_b/index.html) for details.
//!
//! ### `tests/compat/`
//!
//! Tests compatibility with old, published, versions of derive-adhoc.
//! See `tests/compat/README.md`.
//!
//! ### The `#[cfg(test)]` modules listed in `tests/tests.rs`
//!
//! Each module is compiled, and its `#[test]` functions are run.

// We don't want to have to cfg-mark all the imports
#![cfg_attr(
    not(all(test, feature = "full", feature = "ui", feature = "recent")),
    allow(unused_imports, dead_code)
)]

use std::fmt::Debug;
use std::fmt::Display;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter;
use std::path::PathBuf;

use easy_ext::ext;
use educe::Educe;
use itertools::Itertools;
use regex::Regex;

pub mod tutils;
pub use tutils::*;

// These mostly serve to avoid rustdoc warnings from directly::macros,
// which is macros.rs re-built outside the proc macro system.
#[allow(unused_imports)]
use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};

//---------- modules containing straightforwrad `#[test]` tests ----------

#[cfg(test)]
mod list_names;

#[cfg(test)]
mod modules;

// This is special, and contains tests that use a clone of the d-a-m crate
mod directly;
