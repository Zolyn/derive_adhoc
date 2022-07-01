// Here's an example of an attribute that overrides the default behavior
// for an attribute.

// Read "Hash" first for more discussion.

use std::fmt::{self, Debug, Formatter};

use derive_adhoc_macros::define_derive_adhoc;
use derive_adhoc_macros::{derive_adhoc, Adhoc};

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
            f.debug_struct(stringify!($tname))
                $(
                    ${if fmeta(debug(skip)) {
                    } else if fmeta(debug(into)) {
              .field(stringify!($fname),
       &< ${fmeta(debug(into)) as ty} as From<&$ftype> >::from(&self.$fname)
              )
                    } else {
              .field(stringify!($fname), &self.$fname)
                    }}
                )
                .finish()
        }
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
}

// Expands to...

/*
impl Debug for DataType
    where u8: Debug,
          &Vec<String>: Into<PrettyVec>,
          PrettyVec: Debug
{
    fn debug(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.debug_struct("DataType")
            .field("foo", &self.foo)
            .field("bar", <PrettyVec as From<&Vec<String>>>::from(&self.bar))
            .finish()
    }
}
*/

fn main() {
    let dt = DataType {
        foo: 42,
        bar: ["a", "bar"].iter().map(|s| s.to_string()).collect(),
        opaque: Opaque,
    };

    println!("dt = {:?}", &dt);
}
