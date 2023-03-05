// Tests that cause internal macros to generate their "incompatible versions"
// advice.  Actually generating this advice in a more realistic scenario,
// rather than just directly invoking the macros, is quite hard.  So this
// test is not particularly realistic.  But it does exercise the error
// generation code paths.

use derive_adhoc::{define_derive_adhoc, Adhoc};

#[derive(Adhoc)]
struct Driver;

define_derive_adhoc! {
    Template =
}

derive_adhoc_driver_Driver!{ GARBAGE -> DRIVER }
derive_adhoc_template_Template!{ GARBAGE -> TEMPLATE }
derive_adhoc::derive_adhoc_expand!{ GARBAGE -> INNARDS }

fn main() {}
