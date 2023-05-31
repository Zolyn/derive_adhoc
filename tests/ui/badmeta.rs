use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
#[adhoc(wrong = "{")]
struct DataType {}

derive_adhoc! {
    DataType:

    const K: () = ${tmeta(wrong)}
}

fn main() {}
