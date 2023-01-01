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
    f: FieldType,
}

derive_adhoc! {
    ExpandName:

    struct ${paste "Pre" $ttype "Post"} {
    }
}
