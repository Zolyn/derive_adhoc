
use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
struct DataType;

derive_adhoc! {
    DataType with unknown option:
}

derive_adhoc! {
    DataType for wombat:
}

derive_adhoc! {
    DataType for union:
}

derive_adhoc! {
    DataType for struct:
}

fn main() {}
