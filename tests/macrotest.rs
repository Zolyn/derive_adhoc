#[cfg(feature = "macrotest")]
#[test]
pub fn macrotest_expand() {
    macrotest::expand("expand/*.rs");
}
