//! Simple example, including use of an #[adhoc(...)] attribute
//!
//! Also demonstrates use of field type trait bounds.
use derive_adhoc::define_derive_adhoc;
use derive_adhoc::{derive_adhoc, Adhoc};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
#[derive_adhoc(MyHash)]
struct DataType {
    foo: u8,
    #[adhoc(hash(skip))]
    bar: Vec<String>,
}
impl Hash for DataType
where
    u8: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.foo.hash(state);
        self.bar.hash(state);
    }
}
#[derive_adhoc(MyHash)]
struct Pair<S, T: Debug>
where
    S: Debug,
{
    first: S,
    second: T,
}
impl<S, T: Debug> Hash for Pair<S, T>
where
    S: Debug,
    S: Hash,
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.first.hash(state);
        self.second.hash(state);
    }
}
#[derive_adhoc(MyHash)]
struct IntPair(usize, usize);
impl Hash for IntPair
where
    usize: Hash,
    usize: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}
fn main() {
    let v = DataType {
        foo: 23,
        bar: <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new(["hi".into()])),
    };
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    v.hash(&mut hasher);
    {
        ::std::io::_print(
            ::core::fmt::Arguments::new_v1(
                &["", "\n"],
                &[::core::fmt::ArgumentV1::new_lower_hex(&hasher.finish())],
            ),
        );
    };
}
