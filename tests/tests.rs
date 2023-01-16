//! Tests for derive-adhoc
//!
//! Internal, unpublished crate.

#![cfg(test)]

// Test cases that we compile and, maybe, run

mod debug;
mod hash;
mod idpaste;
mod list_names;
mod second;

// macrotest.r is handled separately, since it's more complex
// and we would rather run all its stuff in its own process.
