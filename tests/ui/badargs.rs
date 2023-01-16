
use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
struct DataType {
    foo: u8,
    #[adhoc(hash(skip))]
    bar: Vec<String>,
}

derive_adhoc! {
    DataType:

    fn $unknown() { }

    $(
    )

    ${for fields {
        f ${fname junk}() { }
    }}
}

fn main() {
}
