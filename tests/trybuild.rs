#[test]
pub fn run_pass_expand() {
    let t = trybuild::TestCases::new();
    let ignore = glob::Pattern::new("*.expanded.rs").unwrap();
    for path in glob::glob("expand/*.rs").unwrap() {
        let path = path.unwrap();
        if ignore.matches_path(&path) {
            continue;
        }
        t.pass(path);
    }
}

// Probably, don't add a straightforward run-pass/ directory
// and corresponding call to trybuild, here.
//
// Instead, put the file in tests/, with `#[test]` annotations,
// and add a `mod` line to `tests/tests.rs`, as with `list_names.rs`.

#[test]
pub fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("ui/*.rs");
}
