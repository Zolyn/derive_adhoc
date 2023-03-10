// See tests/stderr/Cargo.toml, under `[features]`
#![cfg(all(feature = "enable", not(feature = "disable")))]
//! See `tests/stderr.rs`

#[path = "../ui/badoptions.rs"]
mod badoptions;
