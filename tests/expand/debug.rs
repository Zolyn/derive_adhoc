// Here's an example of an attribute that overrides the default behavior
// for an attribute.

// Read "hash.rs" first for more discussion.

use std::fmt::{self, Debug, Formatter};

use derive_adhoc::define_derive_adhoc;
use derive_adhoc::{derive_adhoc, Adhoc};

use derive_adhoc_tests::*;

define_derive_adhoc! {
    MyDebug =

    impl Debug for $ttype
    where $(
        ${if fmeta(debug(skip)) {
        } else if fmeta(debug(into)) {
            for <'x> &'x $ftype : Into< ${fmeta(debug(into)) as ty} >,
                ${fmeta(debug(into)) as ty} : Debug,
        } else {
            $ftype: Debug,
        }}
      ) // [1]
    {
        fn fmt(&self, f: &mut Formatter<'_>)
                 -> Result<(), fmt::Error>
        {
            #[allow(unused_mut)]
            let mut ds = f.debug_struct(stringify!($tname));
                $(
                    ${if fmeta(debug(skip)) {
                    } else if fmeta(debug(call)) {
       ${fmeta(debug(call))} (&mut ds, stringify!($fname), &self.$fname)?;
                    } else if fmeta(debug(into)) {
              ds.field(stringify!($fname),
       &< ${fmeta(debug(into)) as ty} as From<&$ftype> >::from(&self.$fname)
              );
                    } else {
              ds.field(stringify!($fname), &self.$fname);
                    }}
                )
                ds.finish()
        }
    }
}

mod custom {
    use super::*;

    pub fn fmt(ds: &mut fmt::DebugStruct, name: &'static str, value: &char)
               -> fmt::Result
    {
        ds.field(name, &(*value as u32));
        Ok(())
    }
}

#[derive(Debug)]
struct PrettyVec<T>(Vec<T>);

impl<T: Clone> From<&Vec<T>> for PrettyVec<T> {
    fn from(v: &Vec<T>) -> Self {
        PrettyVec(v.clone())
    }
}

struct Opaque;

#[derive(Adhoc)]
#[derive_adhoc(MyDebug)]
#[allow(dead_code)]
struct DataType {
    foo: u8,
    #[adhoc(debug(into = "PrettyVec<String>"))]
    bar: Vec<String>,
    #[adhoc(debug(skip))]
    opaque: Opaque,
    #[adhoc(debug(call="custom::fmt"))]
    custom: char,
}

fn main() {
    let dt = DataType {
        foo: 42,
        bar: ["a", "bar"].iter().map(|s| s.to_string()).collect(),
        opaque: Opaque,
        custom: 'y',
    };

    assert_eq!(
        DebugExt::to_debug(&dt),
        "DataType { foo: 42, bar: PrettyVec([\"a\", \"bar\"]), custom: 121 }",
    );
}
