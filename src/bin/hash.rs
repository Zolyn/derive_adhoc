// Here's a simple syntax for the Hash attribute.  I'm using it to
// imagine what parameters look like, and how a "skip" attribute might
// look, and how a "hash_with" attrbite might look.

//#![feature(trace_macros)]
//trace_macros!(true);

use derive_adhoc_macros::define_derive_adhoc;
use derive_adhoc_macros::{derive_adhoc, derive_adhoc_expand, Adhoc};

// use std::fmt::Debug;
use std::hash::{Hash, Hasher};

define_derive_adhoc! {
    MyHash /*for struct*/ =  // [1]

    impl Hash for $ttype
    where $( ${when not(fmeta(hash(skip)))}
             $ftype : Hash , )
    {
        fn hash<H : Hasher>(&self, state: &mut H) {
            $( ${when not(fmeta(hash::skip))}
               self.$fname.hash(state); )
        }
    }
}

#[derive(Adhoc)]
//#[derive_adhoc(MyHash)]
#[derive_adhoc(MyHash)]
struct DataType {
    foo: u8,
    #[adhoc(hash(skip))]
    bar: Vec<String>,
}

/*
#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct Pair<S,T:Debug>
    where S: Debug
{
    first: S,
    second: T,
}
*/

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct IntPair(usize, usize);

// [1] The "for struct" syntax here means that only structs are supported.

// This should expand to:

/*
impl Hash for DataType where u8 : Hash {
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.foo.hash(state);
    }
}

impl<S,T> Hash for Pair<S,T> where
    S: Hash, T: Hash, S: Debug, T: Debug // [3]
{
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.first.hash(state);
        self.second.hsah(state);
    }
}

impl Hash for IntPair
    where usize: Hash, usize: Hash
{
    fn hash<H : Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}
*/

// [3] Note that the "impl..where .. {}" generation here is completely magical.
//    It needs to add the <S,T> after the impl,
//    and it needs to add S:Debug and T:Debug.

fn main() {
    let v = DataType {
        foo: 23,
        bar: vec!["hi".into()],
    };
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    v.hash(&mut hasher);
    println!("{:x}", hasher.finish());
}
