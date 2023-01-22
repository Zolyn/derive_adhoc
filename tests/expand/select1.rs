// Here's a simple syntax for the Hash attribute.  I'm using it to
// imagine what parameters look like, and how a "skip" attribute might
// look, and how a "hash_with" attrbite might look.

//#![feature(trace_macros)]
//trace_macros!(true);

use std::fmt::Debug;

use derive_adhoc::{derive_adhoc, Adhoc};

use derive_adhoc_tests::*;

#[derive(Adhoc, Default, Debug)]
struct Both {
    #[adhoc(left)]
    a: usize,
    #[adhoc(right)]
    b: usize,
    #[adhoc(right)]
    c: usize,
}

derive_adhoc!{
    Both:
    
    #[derive(Default, Debug)]
    struct Left {
        $(
            ${select1
              fmeta(left) { $fname: $ftype, }
              fmeta(right) { }
            }
        )
    }
    #[derive(Default, Debug)]
    struct Right {
        $(
            ${select1
              fmeta(left) { }
              fmeta(right) { $fname: $ftype, }
            }
        )
    }
}

fn main() {
    assert_eq!(
        Left::default().to_debug(),
        "Left { a: 0 }",
    )
}
