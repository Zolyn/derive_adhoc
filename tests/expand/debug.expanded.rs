use std::fmt::{self, Debug, Formatter};
use derive_adhoc_macros::define_derive_adhoc;
use derive_adhoc_macros::{derive_adhoc, Adhoc};
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
}
impl Debug for DataType
where
    u8: Debug,
    for<'x> &'x Vec<String>: Into<PrettyVec<String>>,
    PrettyVec<String>: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_struct("DataType")
            .field("foo", &self.foo)
            .field("bar", &<PrettyVec<String> as From<&Vec<String>>>::from(&self.bar))
            .finish()
    }
}
fn main() {
    let dt = DataType {
        foo: 42,
        bar: ["a", "bar"].iter().map(|s| s.to_string()).collect(),
        opaque: Opaque,
    };
    {
        ::std::io::_print(
            ::core::fmt::Arguments::new_v1(
                &["dt = ", "\n"],
                &[::core::fmt::ArgumentV1::new_debug(&&dt)],
            ),
        );
    };
}
