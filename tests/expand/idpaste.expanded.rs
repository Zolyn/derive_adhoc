//! Test name macros and several scopes of iteration.
#![allow(dead_code)]
use derive_adhoc::{derive_adhoc, Adhoc};
struct TypeNames {
    error: std::slice::Chunks<'static, ()>,
}
struct PreTypeNamesPost {
    error: std::slice::RChunksMut<'static, ()>,
}
struct TopName<F> {
    f: F,
}
struct PreTopNamePost<F> {
    f: F,
}
