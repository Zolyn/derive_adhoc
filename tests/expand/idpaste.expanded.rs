//! Test name macros and several scopes of iteration.
#![allow(dead_code)]
use derive_adhoc::{derive_adhoc, Adhoc};
type FieldType = ();
struct TypeNames {
    error: std::slice::Chunks<'static, ()>,
}
struct PreTypeNamesPost {
    error: std::slice::RChunksMut<'static, ()>,
}
struct TopName<F> {
    top_name_field: F,
}
struct PreTopNamePost<F> {
    TopNameField: F,
}
struct ExpandName {
    #[adhoc(prefix = "attr", suffix = 24)]
    f: FieldType,
    k: String,
}
struct PreExpandNamePost {
    attr_f_24: FieldType,
    k: String,
}
fn main() {}
