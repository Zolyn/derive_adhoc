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

use std::path::PathBuf;

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

/// List the test cases in tests/expand/*.rs
///
/// Filters out tests with the word `recent` in,
/// unless the `recent` cargo feature is enabled.
/// These are tests that won't work with our MSRV.
///
/// Filters out *.expanded.rs.
pub fn list_expand_test_paths() -> impl Iterator<Item = PathBuf> {
    let ignores: Vec<_> = [
        "*.expanded.rs",
        #[cfg(not(feature = "recent"))]
        "*recent*",
    ]
        .iter()
        .map(|pat| glob::Pattern::new(pat).unwrap())
        .collect();

    glob::glob("expand/*.rs")
        .unwrap()
        .filter_map(move |path| {
            let path = path.unwrap();
            if ignores.iter().any(|pat| pat.matches_path(&path)) {
                return None;
            }
            Some(path)
        })
}
