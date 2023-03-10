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

// OpCompatVersions 1.0, ie our own, and the earliest released
derive_adhoc::derive_adhoc_expand!{
    { pub struct StructName {} }
    [1 0]
    { }
    { pub struct OptOk0; }
    { crate; [] Template; }
}

// OpCompatVersions 1.200, some imaginary future compatible one
derive_adhoc::derive_adhoc_expand!{
    { pub struct StructName {} }
    [1 200]
    { }
    { pub struct OptOk200; }
    { crate; [] Template; }
}

// OpCompatVersions 200.1, some imaginary future incompatible one
derive_adhoc::derive_adhoc_expand!{
    { pub struct StructName {} }
    [200 0]
    { }
    { OUGHT NOT TO BE EXPANDED; }
    { crate; [] Template; }
}

fn main() {
    let _ = OptOk0;
    let _ = OptOk200;
}
