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

use std::iter;
use std::path::PathBuf;

#[cfg(test)]
mod list_names;

#[cfg(test)]
mod modules;

#[cfg(test)]
mod stderr;

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
