use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
#[adhoc(wrong = "{")]
#[adhoc(dot = ".")]
struct DataType {}

derive_adhoc! {
    DataType:

    const K: () = ${tmeta(wrong)}
}
// Invoke derive_adhoc! a second time, because (empirically) the
// attempt to parse "{" causes some kind of nonlocal exit:
// syn::LitStr::parse is called but doesn't return.  And then, something
// arranges to report only the *last* such error.  But we want to see
// all the errors, so invoke the macro separately.

derive_adhoc! {
    DataType:

    struct ${paste Bad ${tmeta(dot) as str}};
}

fn main() {}
