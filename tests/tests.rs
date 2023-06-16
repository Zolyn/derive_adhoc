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

use std::fmt::Debug;
use std::iter;
use std::path::PathBuf;

use easy_ext::ext;

//---------- modules containing straightforwrad `#[test]` tests ----------

#[cfg(test)]
mod list_names;

#[cfg(test)]
mod modules;

//---------- utilities and common code for testing ----------

#[ext(DebugExt)]
pub impl<T: Debug> T {
    fn to_debug(&self) -> String {
        format!("{:?}", self)
    }
}

const EXPAND_TEST_GLOB: &str = "expand/*.rs";
const EXPAND_MACROTEST_IGNORE_GLOB: &str = "*.expanded.rs";

/// List the test cases in tests/expand/*.rs
///
/// Filters out tests with the word `recent` in,
/// unless the `recent` cargo feature is enabled.
/// These are tests that won't work with our MSRV.
///
/// Filters out *.expanded.rs.
pub fn list_expand_test_paths() -> Vec<PathBuf> {
    list_expand_general(&|_| false)
}

/// List the test cases in tests/expand/*.rs, for `macrotest`
///
/// Like `list_expand_test_paths`, but if the only thing we filter
/// out is `*.expanded.rs`, simply returns the `expand/*.rs` glob.
///
/// Then macrotest will automatically ignore `*.expanded.rs`.
/// We do this because macrotest is faster and less noisy
/// if we run it once with a single glob.
pub fn list_expand_test_paths_for_macrotest() -> Vec<PathBuf> {
    list_expand_general(&|ign| ign == EXPAND_MACROTEST_IGNORE_GLOB)
}

fn list_expand_general(will_filter: &dyn Fn(&str) -> bool) -> Vec<PathBuf> {
    let ignores = [
        EXPAND_MACROTEST_IGNORE_GLOB,
        #[cfg(not(feature = "recent"))]
        "*recent*",
    ];

    if ignores.iter().cloned().all(will_filter) {
        return iter::once(EXPAND_TEST_GLOB.into()).collect();
    }

    let ignores: Vec<_> = ignores
        .iter()
        .map(|pat| glob::Pattern::new(pat).unwrap())
        .collect();

    glob::glob(EXPAND_TEST_GLOB)
        .unwrap()
        .filter_map(move |path| {
            let path = path.unwrap();
            if ignores.iter().any(|pat| pat.matches_path(&path)) {
                return None;
            }
            Some(path)
        })
        .collect()
}
