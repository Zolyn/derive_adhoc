// Here's an example of an attribute that overrides the default behavior
// for an attribute.

// Read "Hash" first for more discussion.

#[derive(Adhoc)]
#[derive_adhoc(MyDebug)]
struct DataType {
    foo: u8,
    #[adhoc(debug(into="PrettyVec"))]
    bar: Vec<String>,
    #[adhoc(debug(skip))]
    opaque: Opaque,
}

define_derive_adhoc!{
    MyDebug  => {
        impl std::fmt::Debug for $type
            where $( ${if not(attr(debug::skip))
                       ${if attr(debug::into) {
                           &$ty : std::convert::Into<${attr::debug::into as ty}> + // [0]
                           ${attr::debug::into as ty} : std::fmt::Debug
                       } else { // [1]
                           $ty: std::fmt::Debug
                       }}}
                     )"+"+ //[2]
        {
            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> : std::hash::Hasher>(&self, state: &mut H) {
                f.debug_struct(${string $typename}) // [3] [4]
                $(
                    ${if attr(debug::skip) {
                    } elseif attr(debug::info) { // [1]
                        .field(${string $field},
                               <${attr::debug::into as ty} as From<&$ty>>::from(&self.field)
                    } else {
                        .field(${string $field}, &self.$field)
                    }}
                )*
                    .finish()
            }
        }
    }
    }
}

// [0] I'm adding an "interpolate the value of this attribute" syntax as
//     ${attr::PATHNAME (as SYNTAX)?}
// [1] I'm expanding the syntax of ${if ...} here to:
//     ${if expr { content } (elseif expr { content })* (else {content})?}
// [2] The $( .. )"+"+ syntax here isn't going to work with the "debug(skip)"
//     case: it will put multiple +s in a row.  With this combined with the
//     problem of making "where" optional, I'm thinking that the where clause
//     on an impl needs to be magic.
// [3] I've introduced ${string expr} to represent the stringification of an
//     input.
// [4] I've introduced $typename as the name of the type, not including
//     generic parameters.


// Expands to...

impl std::fmt::Debug for DataType
    where u8: std::fmt::Debug +
          &Vec<String>: std::convert::Into<PrettyVec>,
          PrettyVec: std::fmt::Debug
{
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("DataType")
            .field("foo", &self.foo)
            .field("bar", <PrettyVec as From<&Vec<String>>>::from(&self.bar))
            .finish()
    }

}
