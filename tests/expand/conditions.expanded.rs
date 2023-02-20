//! Test cases for conditions
//!
//! This gives a basic demonstration of how to handle an enum.
use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};
trait Trait {
    fn shape_top(&self) -> &'static str;
    fn shape_fields(&self) -> &'static str;
    fn has_vmeta(&self) -> bool;
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
    fn has_vmeta(&self) -> bool {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Unit {} => false,
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
    fn has_vmeta(&self) -> bool {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Tuple { 0: f_0 } => false,
            }
        }
    }
}
#[derive_adhoc(Trait)]
#[adhoc(hello(there = 42))]
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
    fn has_vmeta(&self) -> bool {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Struct { field: f_field } => true,
            }
        }
    }
}
#[derive_adhoc(Trait)]
enum Enum {
    #[adhoc(hello(there))]
    Unit,
    #[adhoc(hello(there(inner)))]
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
    fn has_vmeta(&self) -> bool {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Enum::Unit {} => true,
                #[allow(unused_variables)]
                Enum::Tuple { 0: f_0 } => true,
                #[allow(unused_variables)]
                Enum::Named { field: f_field } => false,
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
    fn has_vmeta(&self) -> bool {
        #[allow(unused_unsafe)]
        unsafe {
            match self {
                #[allow(unused_variables)]
                Union { field: f_field } => false,
            }
        }
    }
}
fn static_test() {}
fn test(top: &str, fields: &str, vmeta: bool, v: impl Trait) {
    if !(v.shape_top() == top && v.shape_fields() == fields && v.has_vmeta() == vmeta) {
        { ::std::rt::begin_panic("explicit panic") }
    }
}
fn main() {
    static_test();
    test("struct", "unit", false, Unit);
    test("struct", "tuple", false, Tuple(0));
    test("struct", "named", true, Struct { field: 0 });
    test("enum", "unit", true, Enum::Unit);
    test("enum", "tuple", true, Enum::Tuple(0));
    test("enum", "named", false, Enum::Named { field: 0 });
    test("union", "named", false, Union { field: 0 });
}
