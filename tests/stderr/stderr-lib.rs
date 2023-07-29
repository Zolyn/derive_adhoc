// See tests/stderr/Cargo.toml, under `[features]`
#![cfg(not(feature = "disable"))]
//! See `tests/stderr.rs`

#[cfg(feature = "enable-main")]
#[path = "../ui/badoptions.rs"]
mod badoptions;

// Only compare the output using the recent versions, since the
// minimal-versions produce different output
// (mostly, different formatting for `$foo`, `$ foo`).
#[cfg(feature = "enable-recent")]
#[path = "../expand/dbg-all-keywords.rs"]
mod dbg_all_keywords;

// Again, minimal-versions produces slightly different output
#[cfg(feature = "enable-recent")]
#[path = "../expand/ref-version.rs"]
#[macro_use]
mod ref_version;

#[cfg(feature = "enable-recent")]
mod dbg_recent {
    derive_adhoc::derive_adhoc! {
        EnumReference:

        $dbg_all_keywords
    }
}
