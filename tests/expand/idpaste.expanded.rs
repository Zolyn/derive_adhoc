//! Examples / test cases for identifier pasting.
//!
//! Refer to `idpaste.expanded.rs` to see what this generates.
#![allow(dead_code)]
use derive_adhoc::{derive_adhoc, Adhoc};
type FieldType = ();
struct TypeNames {
    /// We use std::slice::Chunks here because that way we can test
    /// identifier pasting with a whole path.  The macro `TypeNames`
    /// will generate a field with type `RChunksMut.
    error: std::slice::Chunks<'static, ()>,
}
struct PreTypeNamesPost {
    error: std::slice::RChunksMut<'static, ()>,
}
struct TopName<F> {
    top_name_field: F,
}
#[allow(non_snake_case)]
struct PreTopNamePost<F> {
    TopNameField: F,
}
impl<F> std::panic::RefUnwindSafe for PreTopNamePost<F> {}
struct ExpandName {
    #[adhoc(prefix = "attr", suffix = 24)]
    f: FieldType,
    k: String,
}
struct PreExpandNamePost {
    attr_f_24: FieldType,
    k: String,
}
struct WomExpandNameBat;
fn main() {}
