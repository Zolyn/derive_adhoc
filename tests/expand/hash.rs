//! Simple example, including use of an #[adhoc(...)] attribute
//!
//! Also demonstrates use of field type trait bounds.

use derive_adhoc::define_derive_adhoc;
use derive_adhoc::{derive_adhoc, Adhoc};

use std::fmt::Debug;
use std::hash::{Hash, Hasher};

define_derive_adhoc! {
    /// Derives `Hash`
    MyHash =

    impl<$tgens> Hash for $ttype
    where $twheres
          $( ${when not(fmeta(hash(skip)))}
             $ftype : Hash , )
    {
        fn hash<H : Hasher>(&self, state: &mut H) {
            $( ${when not(fmeta(hash(skip)))}
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
struct Pair<S, T: Debug>
where
    S: Debug,
{
    first: S,
    second: T,
}

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct IntPair(usize, usize);

fn main() {
    let v = DataType {
        foo: 23,
        bar: vec!["hi".into()],
    };
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    v.hash(&mut hasher);
    println!("{:x}", hasher.finish());
}
