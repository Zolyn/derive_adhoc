use derive_adhoc::{define_derive_adhoc, Adhoc};

define_derive_adhoc! { Dbg = $dbg_all_keywords }

#[allow(dead_code)]
#[derive(Adhoc)]
#[derive_adhoc(Dbg)]
enum Enum {
    Unit,
    Tuple(usize),
    Struct { field: String },
}

#[allow(dead_code)]
#[derive(Adhoc)]
#[derive_adhoc(Dbg)]
struct Unit;

#[allow(dead_code)]
#[derive(Adhoc)]
#[derive_adhoc(Dbg)]
struct Tuple(usize);

#[allow(dead_code)]
#[derive(Adhoc)]
#[derive_adhoc(Dbg)]
struct Struct {
    field: String,
}

fn main() {}
