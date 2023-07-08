//! utilities and common code for testingy

use super::*;

#[ext(DebugExt)]
pub impl<T: Debug> T {
    fn to_debug(&self) -> String {
        format!("{:?}", self)
    }
}

/// Compile `re` into a [`Regex`], with cacheing
// Not using lazy_regex because we sometimes make our
// regexps with format! so they aren't static
#[cfg(feature = "recent")] // Doesn't work on 1.54; Mutex::new isn't const
pub fn compile_re_cached(re: &str) -> Regex {
    use std::sync::Mutex;
    static CACHE: Mutex<Option<HashMap<String, Regex>>> = Mutex::new(None);

    let mut guard = CACHE.lock().unwrap();
    let cache = guard.get_or_insert_with(|| Default::default());
    match cache.get(re) {
        None => {
            let re = Regex::new(re).expect(&format!("bad regexp {re}"));
            cache.entry(re.to_string()).or_insert(re)
        }
        Some(re) => re,
    }
    .clone()
}

/// `fn re!(l: &str) -> Regex`
#[macro_export]
macro_rules! re { { $re:expr $(,)? } => {
    compile_re_cached($re.as_ref())
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
