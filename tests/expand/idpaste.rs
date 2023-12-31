//! Examples / test cases for identifier pasting.
//!
//! Refer to `idpaste.expanded.rs` to see what this generates.

#![allow(dead_code, unused_variables)]

use derive_adhoc::{derive_adhoc, Adhoc};

type FieldType = ();

#[derive(Adhoc)]
struct TypeNames {
    /// We use std::slice::Chunks here because that way we can test
    /// identifier pasting with a whole path.  The macro `TypeNames`
    /// will generate a field with type `RChunksMut.
    error: std::slice::Chunks<'static, ()>,
}

derive_adhoc! {
    TypeNames:

    struct ${paste Pre $tdeftype Post} {
        $(
            $fname: ${paste R $ftype Mut},
        )
    }
}

#[derive(Adhoc)]
struct TopName<F> {
    top_name_field: F,
}

derive_adhoc! {
    TopName:

    #[allow(non_snake_case)]
    struct ${paste Pre $tdeftype Post} {
        $( ${pascal_case $fname}: $ftype )
    }

    impl<$tgens> std::panic::RefUnwindSafe for ${paste Pre $ttype Post} {}
}

#[derive(Adhoc)]
struct ExpandName {
    #[adhoc(prefix = "attr", suffix = "24")]
    f: FieldType,

    k: String,
}

derive_adhoc! {
    ExpandName:

    struct ${paste "Pre" $tdeftype "Post"} {
        $(
            ${paste
              ${if fmeta(prefix) { ${fmeta(prefix)} _ } }
              $fname
              ${if fmeta(suffix) { _ ${fmeta(suffix) as str} } }
            }: $ftype,
        )
    }

    struct ${pascal_case ${paste wom_ ${shouty_snake_case $tname}
                           ${if true { _bat } else { _noise }}}};
}

#[derive(Adhoc)]
enum EdgeCases {
    Tuple(u32),
    Struct { r#for: u32, r#unneeded: u32 },
}

derive_adhoc! {
    EdgeCases:

    impl $ttype {
        fn $<edge_ $<"0" _end>>() {}
        fn $<edge_ $( $fname )>() {}
        fn $<$tdefkwd>() {}
        $(
            fn $<binding_ $<$fname> _end>() {}
            // `: `ok_0_tail`, `ok_field_tail`
        )
        fn body() {
            $(
                ${when v_is_named}
                let $fname = ();
                let $<$fname> = ();
            )
        }
    }
}

fn main() {}
