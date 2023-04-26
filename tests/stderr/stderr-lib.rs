// See tests/stderr/Cargo.toml, under `[features]`
#![cfg(not(feature = "disable"))]
//! See `tests/stderr.rs`

#[cfg(feature = "enable-main")]
#[path = "../ui/badoptions.rs"]
mod badoptions;
