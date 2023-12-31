use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
#[adhoc(something = "Box<char>")]
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

derive_adhoc! {
    DataType:

    struct ${paste $tname _ 42};
}

derive_adhoc! {
    DataType:

    struct ${paste $ttype ${tmeta(something) as ty}};
}

derive_adhoc! {
    DataType:

    // This expands to "r#struct Broken { }"
    ${paste tdefkwed} Broken { }
}

fn main() {}
