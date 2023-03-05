// Tests involving driver and template options

use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};

define_derive_adhoc! {
    BadOptionsTemplate =
    broken template;
}

#[derive(Adhoc)]
#[derive_adhoc(BadOptionsTemplate[dbg])]
struct BadOptionsDriver;

derive_adhoc! {
    BadOptionsDriver with unknown option:
}

derive_adhoc! {
    BadOptionsDriver for wombat:
}

derive_adhoc! {
    BadOptionsDriver for union:
}

derive_adhoc! {
    BadOptionsDriver for struct:
}

derive_adhoc! {
    BadOptionsDriver dbg:
    syntax error;
}

derive_adhoc! {
    BadOptionsDriver dbg, for struct, for union:
}

fn main() {}
