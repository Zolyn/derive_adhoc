
use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
struct DataType;

derive_adhoc! {
    DataType with unknown option:
}

fn main() {}
