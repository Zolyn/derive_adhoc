// Tests of syntax we may wish to use for things in the future

use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};

#[derive(Adhoc)]
struct ReserveDriver;

derive_adhoc! {
    ReserveDriver:
    $[ future ]
}

derive_adhoc! {
    ReserveDriver:
    $<ident:upper>
    ${paste ident:upper}
}

derive_adhoc! {
    ReserveDriver:
    $r#"template content?"#
}

derive_adhoc! {
    ReserveDriver:

    mod prevent {
        //! template doc comment?
    }
}

derive_adhoc! {
    ReserveDriver:

    $/// template doc comment?
    mod prevent {
    }
}

derive_adhoc! {
    ReserveDriver:

    ${define 0 { mod x {} }}
    $0
}

fn main() {}
