#[allow(unused_imports)]
use derive_adhoc_tests::*;

// We don't run this unless the CI explicitly enables it.
// That's done only for the "cargo-test" test,
// ie our pinned nightly, with full features enabled.
//
// We don't do expansion match tests other than on our pinned compiler.
//
// We don't do these tests without all derive-adhoc features enabled.
// So there is n testing of what feature controls what.
// That's OK, we're not going to have very many features.
#[cfg(feature = "macrotest")]
#[test]
pub fn macrotest_expand() {
    for path in list_expand_test_paths_for_macrotest() {
        macrotest::expand_args(path, ["--all-features"]);
    }
}
