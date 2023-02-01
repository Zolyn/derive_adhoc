//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT

// This builds on hash.rs, but also works on enums.


#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
enum Enum {
    Case1,
    Case2 {
        a: u8,
        #[attr(hash(skip))]
        unhashed: String,
    },
    Case3(u8, u8),
}

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct Basic {
    a: u8,
    b : u8
}

#[derive(Adhoc)]
#[derive_adhoc(MyHash)]
struct Tuple(u8, u8);

define_derive_adhoc!{
    MyHash on any =  // [1]
        impl Hash for $ttype
            where $($( ${when not(fattr(hash::skip))}
                     $ftype : Hash + )) // [2]
        {
            fn hash<H : Hasher>(&self, state: &mut H) {
                match self {
                    $(
                        $vpat => { // [3]
                            $(${when enum} // [3a]
                                stringify!($vname).hash(state); )// [4]
                            $(${when not(attr(hash::skip))}
                              $pfname.hash(state); // [5]
                            )
                        }
                    )
                }
            }
        }
}

// [1] Here "on any" means that we intend this to work on enums and
//    structs.  Structs in this context are implicitly treated close
//    to single-variant enums.
// [2] Note that with an enum, we have to nest two levels deep to see the
//     types.
// [3] $vpat is a pattern that deconstructs the current variant.  If the
//    current type is a struct, it deconstruct the struct.
// [3a] enum is an expression that is true iff the currrent type is an enum.
// [4] $vname is the current variant name, without any fields.  If the
//     current type is not an enum, it fails to expand.
// [5] $pfname is the name of a field within $vpat.

// This expands to:

impl Hash for Enum
    where u8: Hash + u8: Hash + u8: Hash +
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Case1 => {
                "Case1".hash(state);
            }
            Case2 { a, unhashed } => {
                "Case2".hash(state);
                a.hash(state);
                // XXX do we care about "unhashed is unused" warnings?
            }
            Case3(_0, _1) => {
                "Case3".hash(state);
                _0.hash(state);
                _1.hash(state);
            }
        }
    }
}

impl Hash for Basic
    where u8: Hash + u8: Hash +
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Basic { a, b } => { a.hash(state); b.hash(state); }
        }
    }
}

impl Hash for Tuple
    where u8: Hash + u8: Hash +
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Tuple(_0, _1) => { _0.hash(state); _1.hash(state); }
        }
    }
}

