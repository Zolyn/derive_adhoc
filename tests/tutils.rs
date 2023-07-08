//! utilities and common code for testingy

use super::*;

#[ext(DebugExt)]
pub impl<T: Debug> T {
    fn to_debug(&self) -> String {
        format!("{:?}", self)
    }
}

/// `fn re!(l: &str) -> Regex`
#[macro_export]
macro_rules! re { { $re:expr $(,)? } => {
    match $re {
        re => {
            use once_cell::sync::OnceCell;
            static RE: OnceCell<Regex> = OnceCell::new();
            RE.get_or_init(|| 
                Regex::new(&re).expect(&format!("bad regexp {re}"))
            )
        }
    }
} }
/// `fn m!(l: &str, re: &str) -> bool`: does regexp `re` match in `l` ?
#[macro_export]
macro_rules! m { { $l:expr, $re:expr $(,)? } => {
    re!($re).is_match(&$l)
} }
/// `fn mc!(l: &str, re: &str) -> Option<(CAP,...)>`: regexp captures?
///
/// `(CAP,...)` is a tuple of `String`.
#[macro_export]
macro_rules! mc { { $l:expr, $re:expr $(,)? } => {
    re!($re)
        .captures(&$l)
        .map(|caps| {
            let caps = caps
                .iter()
                .map(|m| m.map(|m| m.as_str().to_owned()).unwrap_or_default())
                .collect_vec();
            let len = caps.len();
            caps
                .into_iter()
                .skip(1)
                .collect_tuple()
                .unwrap_or_else(|| {
                    panic!("wrong # matches: got {} from {}", len, $re)
                })
        })
} }

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
