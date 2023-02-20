//! Test cases for conditions
//!
//! This gives a basic demonstration of how to handle an enum.
use derive_adhoc::{define_derive_adhoc, Adhoc};
trait Trait {
    fn shape_top(&self) -> &'static str;
    fn shape_fields(&self) -> &'static str;
}
#[derive_adhoc(Trait)]
struct Unit;
impl Trait for Unit {
    fn shape_top(&self) -> &'static str {
        "struct"
    }
    fn shape_fields(&self) -> &'static str {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Unit {} => "unit",
            }
        }
    }
}
#[derive_adhoc(Trait)]
struct Tuple(usize);
impl Trait for Tuple {
    fn shape_top(&self) -> &'static str {
        "struct"
    }
    fn shape_fields(&self) -> &'static str {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Tuple { 0: f_0 } => "tuple",
            }
        }
    }
}
#[derive_adhoc(Trait)]
struct Struct {
    field: usize,
}
impl Trait for Struct {
    fn shape_top(&self) -> &'static str {
        "struct"
    }
    fn shape_fields(&self) -> &'static str {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Struct { field: f_field } => "named",
            }
        }
    }
}
#[derive_adhoc(Trait)]
enum Enum {
    Unit,
    Tuple(usize),
    Named { field: usize },
}
impl Trait for Enum {
    fn shape_top(&self) -> &'static str {
        "enum"
    }
    fn shape_fields(&self) -> &'static str {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Enum::Unit {} => "unit",
                #[allow(unused_variables)]
                Enum::Tuple { 0: f_0 } => "tuple",
                #[allow(unused_variables)]
                Enum::Named { field: f_field } => "named",
            }
        }
    }
}
#[derive_adhoc(Trait)]
union Union {
    field: usize,
}
impl Trait for Union {
    fn shape_top(&self) -> &'static str {
        "union"
    }
    fn shape_fields(&self) -> &'static str {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Union { field: f_field } => "named",
            }
        }
    }
}
fn test(top: &str, fields: &str, v: impl Trait) {
    if !(v.shape_top() == top && v.shape_fields() == fields) {
        { ::std::rt::begin_panic("explicit panic") }
    }
}
fn main() {
    test("struct", "unit", Unit);
    test("struct", "tuple", Tuple(0));
    test("struct", "named", Struct { field: 0 });
    test("enum", "unit", Enum::Unit);
    test("enum", "tuple", Enum::Tuple(0));
    test("enum", "named", Enum::Named { field: 0 });
    test("union", "named", Union { field: 0 });
}
