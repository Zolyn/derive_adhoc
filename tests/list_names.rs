//! Test name macros and several scopes of iteration.
#![allow(dead_code)]

use derive_adhoc::{define_derive_adhoc, Adhoc};

//XXXX BUG: derive_adhoc_expand should not need to be in-scope!
use derive_adhoc::derive_adhoc_expand;

define_derive_adhoc! {
    ListFields =

    impl $ttype {
        fn who_are_you() -> &'static str {
            stringify!( $tname )
        }

        fn list_fields() -> Vec<&'static str> {
            vec! [ $( stringify!( $fname ) , ) ]
        }
    }
}

// XXXX In the original version of the syntax, I had thought that we might
//   have `$v___` work for structs, and treat structs as if they had
//   only a single variant.
//       If we decide not to do that, we will need a different kind of
//   RepeatOver for $vpat, since structs _should_ have $vpat.
define_derive_adhoc! {
    ListVariants =

    impl $ttype {
        fn list_variants() -> Vec<&'static str> {
            vec! [ $( stringify!( $vname ) , ) ]
        }

        fn list_qualified_fields() -> Vec<(&'static str, &'static str)> {
            vec! [
                ${for fields {
                    (stringify!( $vname), stringify!( $fname )) ,
                }}
            ]
        }

        fn list_variant_fields() -> Vec<(&'static str, Vec<&'static str>)> {
            vec! [ $(
                       (stringify!($vname),
                         vec![ $( stringify!( $fname ), ) ]
                       ),
                    )
            ]
        }
    }
}

#[derive(Adhoc)]
#[derive_adhoc(ListFields)]
struct UnitStruct;

#[derive(Adhoc)]
#[derive_adhoc(ListFields)]
struct SimpleStruct {
    small: u8,
    medium: u16,
    large: String,
}

#[derive(Adhoc)]
#[derive_adhoc(ListFields)]
struct TupleStruct(u8, u16, String);

#[derive(Adhoc)]
#[derive_adhoc(ListFields)]
#[derive_adhoc(ListVariants)]
enum Enum {
    UnitVariant,
    StructVariant { a: u8, b: u16 },
    TupleVariant(u8, u16),
}

#[test]
fn type_names() {
    assert_eq!(UnitStruct::who_are_you(), "UnitStruct");
    assert_eq!(SimpleStruct::who_are_you(), "SimpleStruct");
    assert_eq!(TupleStruct::who_are_you(), "TupleStruct");
    assert_eq!(Enum::who_are_you(), "Enum");
}

#[test]
fn list_fields() {
    assert!(UnitStruct::list_fields().is_empty());
    assert_eq!(
        SimpleStruct::list_fields(),
        vec!["small", "medium", "large"]
    );
    assert_eq!(TupleStruct::list_fields(), vec!["0", "1", "2"]);
    assert_eq!(Enum::list_fields(), vec!["a", "b", "0", "1"]);
}

#[test]
fn list_variants() {
    assert_eq!(
        Enum::list_variants(),
        vec!["UnitVariant", "StructVariant", "TupleVariant"]
    );
    assert_eq!(
        Enum::list_qualified_fields(),
        vec![
            ("StructVariant", "a"),
            ("StructVariant", "b"),
            ("TupleVariant", "0"),
            ("TupleVariant", "1"),
        ]
    );
    assert_eq!(
        Enum::list_variant_fields(),
        vec![
            ("UnitVariant", vec![]),
            ("StructVariant", vec!["a", "b"]),
            ("TupleVariant", vec!["0", "1"]),
        ]
    );
}
