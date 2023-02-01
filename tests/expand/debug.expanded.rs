//! More complex example with multiple, complex, attributes etc.
//!
//! Includes an example of an attribute containing Rust syntax.
//!
//! Read "clone.rs" and "hash.rs" first for simpler examples.
use std::fmt::{self, Debug, Formatter};
use derive_adhoc::define_derive_adhoc;
use derive_adhoc::{derive_adhoc, Adhoc};
use derive_adhoc_tests::*;
mod custom {
    use super::*;
    pub fn fmt(
        ds: &mut fmt::DebugStruct,
        name: &'static str,
        value: &char,
    ) -> fmt::Result {
        ds.field(name, &(*value as u32));
        Ok(())
    }
}
struct PrettyVec<T>(Vec<T>);
#[automatically_derived]
impl<T: ::core::fmt::Debug> ::core::fmt::Debug for PrettyVec<T> {
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_tuple_field1_finish(f, "PrettyVec", &&self.0)
    }
}
impl<T: Clone> From<&Vec<T>> for PrettyVec<T> {
    fn from(v: &Vec<T>) -> Self {
        PrettyVec(v.clone())
    }
}
struct Opaque;
#[derive_adhoc(MyDebug)]
#[allow(dead_code)]
struct DataType {
    foo: u8,
    #[adhoc(debug(into = "PrettyVec<String>"))]
    bar: Vec<String>,
    #[adhoc(debug(skip))]
    opaque: Opaque,
    #[adhoc(debug(call = "custom::fmt"))]
    custom: char,
}
impl Debug for DataType
where
    u8: Debug,
    for<'x> &'x Vec<String>: Into<PrettyVec<String>>,
    PrettyVec<String>: Debug,
    char: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        #[allow(unused_mut)]
        let mut ds = f.debug_struct("DataType");
        ds.field("foo", &self.foo);
        ds.field("bar", &<PrettyVec<String> as From<&Vec<String>>>::from(&self.bar));
        custom::fmt(&mut ds, "custom", &self.custom)?;
        ds.finish()
    }
}
fn main() {
    let dt = DataType {
        foo: 42,
        bar: ["a", "bar"].iter().map(|s| s.to_string()).collect(),
        opaque: Opaque,
        custom: 'y',
    };
    match (
        &DebugExt::to_debug(&dt),
        &"DataType { foo: 42, bar: PrettyVec([\"a\", \"bar\"]), custom: 121 }",
    ) {
        (left_val, right_val) => {
            if !(*left_val == *right_val) {
                let kind = ::core::panicking::AssertKind::Eq;
                ::core::panicking::assert_failed(
                    kind,
                    &*left_val,
                    &*right_val,
                    ::core::option::Option::None,
                );
            }
        }
    };
}
