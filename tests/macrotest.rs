use derive_adhoc_tests::*;

#[cfg(feature = "macrotest")]
#[test]
pub fn macrotest_expand() {
    for path in list_expand_test_paths() {
        macrotest::expand(path);
    }
}
