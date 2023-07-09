//! Examples / test cases for identifier pasting.
//!
//! Refer to `idpaste.expanded.rs` to see what this generates.
#![allow(dead_code, unused_variables)]
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
    #[adhoc(prefix = "attr", suffix = "24")]
    f: FieldType,
    k: String,
}
struct PreExpandNamePost {
    attr_f_24: FieldType,
    k: String,
}
struct WomExpandNameBat;
enum EdgeCases {
    Tuple(u32),
    Struct { r#for: u32, unneeded: u32 },
}
impl EdgeCases {
    fn edge_0_end() {}
    fn edge_0forunneeded() {}
    fn r#enum() {}
    fn binding_0_end() {}
    fn binding_for_end() {}
    fn binding_unneeded_end() {}
    fn body() {
        let r#for = ();
        let r#for = ();
        let unneeded = ();
        let unneeded = ();
    }
}
fn main() {}
