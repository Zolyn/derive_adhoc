use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};

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

    ${if false { } else { } forbidden}

    ${if tmeta(some_path) as lit { }}
}

define_derive_adhoc! {
    Broken =

    type Alias = ${ttype $junk};
}

#[derive(Adhoc)]
#[derive_adhoc(Broken)]
struct ForBroken;

fn main() {}
