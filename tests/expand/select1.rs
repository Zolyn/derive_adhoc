// Here's a simple syntax for the Hash attribute.  I'm using it to
// imagine what parameters look like, and how a "skip" attribute might
// look, and how a "hash_with" attrbite might look.

//#![feature(trace_macros)]
//trace_macros!(true);

use derive_adhoc::{derive_adhoc, Adhoc};

use std::fmt::Debug;

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
              fmeta(left) { $fname: $ftype, } else if
              fmeta(right) { }
            }
        )
    }
    #[derive(Default, Debug)]
    struct Right {
        $(
            ${select1
              fmeta(left) { } else if
              fmeta(right) { $fname: $ftype, }
            }
        )
    }
}

fn main() {
    assert_eq!(
        format!("{:?}", &Left::default()),
        "Left { a: 0 }",
    )
}
