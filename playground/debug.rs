// Here's an example of an attribute that overrides the default behavior
// for an attribute.

// Read "Hash" first for more discussion.

use derive_adhoc_macros::define_derive_adhoc;
use derive_adhoc_macros::{derive_adhoc, derive_adhoc_expand, Adhoc};

define_derive_adhoc!{
    MyDebug =

    impl Debug for $ttype
    where $(
        ${if fattr(debug::skip) {
        } else if fattr(debug(into)) {
            & $ftype : Into<${attr(debug(into)) as ty}> +
                ${attr(debug(into)) as ty} : Debug,
        } else {
            $ftype: Debug,
        }}
      ) // [1]
    {
        fn debug(&self, f: &mut Formatter<'_>)
                 -> Result<(), Error>
        {
            f.debug_struct(stringify!($tname))
                $(
                    ${if fattr(debug(skip)) {
                    } else if fattr(debug(into)) {
              .field(stringify!($fname),
       < ${fattr(debug(into)) as ty} as From<&$ftype> >::from(&self.field)
              )
                    } else {
              .field(stringify($fname), &self.$fname)
                    }}
                )
                .finish()
        }
    }
}

#[derive(Adhoc)]
#[derive_adhoc(MyDebug)]
struct DataType {
    foo: u8,
    #[adhoc(debug(into="PrettyVec"))]
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
