
use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
struct DataType {
    foo: u8,
    bar: Vec<String>,
}

derive_adhoc! {
    DataType:

    ${for fields {
        ${paste $ttype _ $ftype}
    }}
}

fn main() {
}
