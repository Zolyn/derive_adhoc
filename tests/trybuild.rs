#[allow(unused_imports)]
use derive_adhoc_tests::*;

// We don't run this unless the CI explicitly enables it.
// That's done only for the tests with all d-a features enabled,
// Ie, the ones that aren't -minfeatures.
//
// So there is n testing of what feature controls what.
// That's OK, we're not going to have very many features.
//
// The basic tests in tests.rs still run of course.
#[test]
#[cfg(feature = "full")]
pub fn run_pass_expand() {
    let t = trybuild::TestCases::new();

    for path in list_expand_test_paths() {
        t.pass(path);
    }
}

// Probably, don't add a straightforward run-pass/ directory
// and corresponding call to trybuild, here.
//
// Instead, put the file in tests/, with `#[test]` annotations,
// and add a `mod` line to `tests/tests.rs`, as with `list_names.rs`.
//
// We don't run this unless the CI explicitly enables it.
// That's one only for our main test with the pinned compiler,
// and all d-a features enabled.
//
// We basically trust that the features-disabled fallback code
// for handling the disabled feature is good enough.
#[cfg(feature = "ui")]
#[test]
pub fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("ui/*.rs");
}
