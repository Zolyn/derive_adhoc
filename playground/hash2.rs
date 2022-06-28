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

define_derive_adhoc!{
    MyHash on enum = { // [1]
        impl Hash for $self
            where $($( ${when not(attr(hash::skip))}
                     $ty : Hash + ))* // [2]
        {
            fn hash<H : Hasher>(&self, state: &mut H) {
                match self {
                    $(
                        $pat => { // [3]
                            stringify!($variant).hash(state); // [4]
                            $(${when not(attr(hash::skip))}
                              $patfield.hash(state); // [5]
                            )
                        }
                    )
                }
            }
        }
    }
}

// [1] I've written this to apply to enums only.
// [2] Note that with an enum, we have to nest two levels deep to see the
//     types.
// [3] $pat is a pattern that deconstructs the current variant.
// [4] $variant is the current variant, without any fields.
// [5] $patfield is a field within $pat.

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
                unhashed.hash(state);
            }
            Case3(_0, _1) => {
                "Case3".hash(state);
                _0.hash(state);
                _1.hash(state);
            }
        }
    }
}
