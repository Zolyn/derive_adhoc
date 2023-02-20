use derive_adhoc_tests::*;

#[test]
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

#[cfg(feature = "ui")]
#[test]
pub fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("ui/*.rs");
}
