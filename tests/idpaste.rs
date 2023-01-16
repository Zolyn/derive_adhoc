//! Test name macros and several scopes of iteration.

#![allow(dead_code)]

use derive_adhoc::{derive_adhoc, Adhoc};

type FieldType = ();

#[derive(Adhoc)]
struct TypeNames {
    error: std::slice::Chunks<'static, ()>,
}

derive_adhoc! {
    TypeNames:

    struct ${paste Pre $ttype Post} {
        $(
            $fname: ${paste R $ftype Mut},
        )
    }
}

#[derive(Adhoc)]
struct TopName<F> {
    f: F,
}

derive_adhoc! {
    TopName:

    struct ${paste Pre $ttype Post} {
        $( $fname: $ftype )
    }
}

#[derive(Adhoc)]
struct ExpandName {
    #[adhoc(prefix="attr", suffix=24)]
    f: FieldType,

    k: String,
}

derive_adhoc! {
    ExpandName:

    struct ${paste "Pre" $ttype "Post"} {
        $(
            ${paste
              ${if fmeta(prefix) { ${fmeta(prefix)} _ } }
              $fname
              ${if fmeta(suffix) { _ ${fmeta(suffix)} } }
            }: $ftype,
        )
    }
}

fn main() {
}

#[test]
fn test() {
    main()
}
