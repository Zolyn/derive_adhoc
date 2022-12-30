// Here's a simple syntax for the Hash attribute.  I'm using it to
// imagine what parameters look like, and how a "skip" attribute might
// look, and how a "hash_with" attrbite might look.

//#![feature(trace_macros)]
//trace_macros!(true);

use derive_adhoc_macros::define_derive_adhoc;
use derive_adhoc_macros::{derive_adhoc, Adhoc};

use std::fmt::Debug;
use std::hash::{Hash, Hasher};

define_derive_adhoc! {
    MyHash /*for struct*/ =  // [1]

    impl<$tgens> Hash for $ttype
    where $twheres
          $( ${when not(fmeta(hash(skip)))}
             $ftype : Hash , )
    {
        fn hash<H : Hasher>(&self, state: &mut H) {
            $( ${when not(fmeta(hash::skip))}
               self.$fname.hash(state); )
        }
    }
}

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct DataType {
    foo: u8,
    #[adhoc(hash(skip))]
    bar: Vec<String>,
}

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct Pair<S,T:Debug>
    where S: Debug
{
    first: S,
    second: T,
}

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct IntPair(usize, usize);

// [1] The "for struct" syntax here means that only structs are supported.

fn main() {
    let v = DataType {
        foo: 23,
        bar: vec!["hi".into()],
    };
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    v.hash(&mut hasher);
    println!("{:x}", hasher.finish());
}
