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
        impl Debug for $self
            where $(${if attr(debug::skip) {
                    } elseif attr(debug::into {
                       &$ty : Into<${attr::debug::into as ty}> +
                       ${attr::debug::into as ty} : Debug +
                    } else {
                       $ty: Debug +
                    }
                  }) // [1]
        {
            fn debug(&self, f: &mut Formatter<'_>) -> Result<(), Error> : std::hash::Hasher>(&self, state: &mut H) {
                f.debug_struct(stringify!($typename))
                $(${if attr(debug::skip) {
                    } elseif attr(debug::info) {
                        .field(stringify!($field),
                               <${attr::debug::into as ty}} as From<&$ty>>::from(&self.field)
                    } else {
                        .field(stringify($field), &self.$field)
                    }
                })   // [1]
                    .finish()
            }
            }
    }
    }
    }
}

// [1] I'm leaving out the "*" from $()* here, but it seems pretty
// magical to me.


// Expands to...

impl Debug for DataType
    where u8: Debug +
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
