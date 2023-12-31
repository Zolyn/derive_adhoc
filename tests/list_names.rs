//! Test name macros and several scopes of iteration.

use derive_adhoc::{define_derive_adhoc, Adhoc};

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
#[allow(dead_code)]
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
#[allow(dead_code)]
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
