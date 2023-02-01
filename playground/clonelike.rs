//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT

/// This example explores a function that exposes the internals
/// of a struct or enum by cloning them into a new type where all
/// members are public.

/// An object with a corresponding "Internals" type that can be
/// used to get a public view of its inner state.
trait GetInternals {
    type Internals;
    fn get_internals(&self) -> Self::Internals;
}

#[derive(Adhoc)]
#[derive_adhoc(GetInternals)]
struct DataType {
    foo: u8,
    bar: u8,
    #[getinternals(recurse)]
    baz: OtherType,
}


// Easy case: pretend that it's structures only.
define_derive_adhoc!{
    GetInternals for struct =

    pub struct [<$tname Internals>]  // [0] [1]
        where $( ${if fattr(getinternals::recurse) {
                       $ftype: GetInternals
                    } else {
                       $ftype: Clone
                    }
                  } +
               )
    { // [2]
        $(
           pub
           $(${when not(tuple) $fname :}) // [5]
           ${if fattr(getinternals::recurse) {
               <$ftype as GetInternals>::Internals
             } else {
               $ftype
             }
           }
        ) ,
    }
    impl GetInternals for $ttype
        where $( ${if fattr(getinternals::recurse) {
                       $ftype: GetInternals
                    } else {
                       $ftype: Clone
                    }
                  } +
               )
    {
        pub type Internals = [<$tname Internals>]; // [3]
        pub fn get_internals(&self) -> Self::Internals {
            Self::Internals { // [4]
                $( $(${when not(tuple)} $fname : ) // [5]
                   self.$fname , )
            }
        }
    }
}

// [0] I'm assuming that [<>] works the same way as with paste!.
// [1] There will need to be magic here to make the "internals"
//     struct get declared with the right parameters -- or if not magic,
//     some syntax we can use.
// [2] This part of the syntax won't work for declaring tuple structures
//     or unit structures.
// [3] I need some way to get the parameters right on this type; not sure how.
// [4] This constructor syntax  won't actually work for tuples or unit structs.
//     Not sure what to do about that.
// [5] I'm  assuming that 'tuple' is an expression that is true when
//     the current struct is a tuple struct and false otherwise.
//     An alternative would be to provide soemthing like
//     `$fname_colon_unless_tuple`, but that seems silly.


// Harder case: make it work for enums.  This is not actually necessary
// since the members of enums are always public, but let's pretend.
define_derive_adhoc!{
    GetInternals =

    pub $tdecl [<$tname Internals>]  // [10]
        where $( ${if fattr(getinternals::recurse) {
                       $ftype: GetInternals
                    } else {
                       $ftype: Clone
                    }
                  } +
               )
    {
        $(
            $vname { // [11]
                $( ${when not vtuple } $fname : }
                ${if fattr(getinternals::recurse) {
                    <$ftype as GetInternals>::Internals
                  } else {
                    $ftype
                  }
                }
            } ,
        )
    }
    impl GetInternals for $ttype
        where $( ${if fattr(getinternals::recurse) {
                       $ftype: GetInternals
                    } else {
                       $ftype: Clone
                    }
                  } +
               ) // [11a]
    {
        pub type Internals = [<$tname Internals>];
        pub fn get_internals(&self) -> Self::Internals {
            match self {
                $(
                    $vpat => Self::Internals
                        ${ ${when enum} ::$vname } // [12]
                    { // [13]
                        $(
                             $( $(${when not(vtuple)} $fname : ) // [14]
                             $pfname , )
                         )
                    }
                ) *
            }
        }
    }
}

// [10] Adding a `$tdecl` here to mean "the appropriate keyword for declaring
//      the toplevel type.
// [11] This syntax won't work for non-enum types, tuple variants, or
//      unit variants!
// [11a] I am getting tired of copy-pasting this over and over.  Dare
//       I ask for some way to get the regular macro facility to interact
//        with these declarations?
// [12] This is needed to get the variant name included only when
//      there is a variant.
// [13] This syntax, as before, won't work for variants that are tuples
//      or units.
// [14] Adding "vtuple" to mean "this variant is a tuple."  In a struct,
//      vtuple is the same as tuple.

