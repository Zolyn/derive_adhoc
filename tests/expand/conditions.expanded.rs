//! Test cases for conditions
//!
//! This gives a basic demonstration of how to handle an enum.
use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};
trait Trait {
    fn shape_top(&self) -> &'static str;
    fn shape_fields(&self) -> &'static str;
    fn has_tmeta(&self) -> bool;
    fn has_vmeta(&self) -> bool;
}
trait GetUsize {
    fn get_usize(&self) -> Option<usize>;
}
#[derive_adhoc(Trait, GetUsize)]
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
    fn has_tmeta(&self) -> bool {
        false
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
impl GetUsize for Unit {
    fn get_usize(&self) -> Option<usize> {
        match self {
            #[allow(unused_variables)]
            Unit {} => {}
        };
        #[allow(unreachable_code)] None
    }
}
#[derive_adhoc(Trait, GetUsize)]
#[adhoc(hi(ferris))]
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
    fn has_tmeta(&self) -> bool {
        true
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
impl GetUsize for Tuple {
    fn get_usize(&self) -> Option<usize> {
        match self {
            #[allow(unused_variables)]
            Tuple { 0: f_0 } => {
                return Some(*f_0);
            }
        };
        #[allow(unreachable_code)] None
    }
}
#[derive_adhoc(Trait, GetUsize)]
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
    fn has_tmeta(&self) -> bool {
        false
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
impl GetUsize for Struct {
    fn get_usize(&self) -> Option<usize> {
        match self {
            #[allow(unused_variables)]
            Struct { field: f_field } => {
                return Some(*f_field);
            }
        };
        #[allow(unreachable_code)] None
    }
}
#[derive_adhoc(Trait, GetUsize)]
enum Enum {
    #[adhoc(hello(there))]
    Unit,
    #[adhoc(hello(there(inner)))]
    Tuple(usize),
    Named { field: u32 },
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
    fn has_tmeta(&self) -> bool {
        false
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
impl GetUsize for Enum {
    fn get_usize(&self) -> Option<usize> {
        match self {
            #[allow(unused_variables)]
            Enum::Unit {} => {}
            #[allow(unused_variables)]
            Enum::Tuple { 0: f_0 } => {
                return Some(*f_0);
            }
            #[allow(unused_variables)]
            Enum::Named { field: f_field } => {}
        };
        #[allow(unreachable_code)] None
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
    fn has_tmeta(&self) -> bool {
        false
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
fn test(top: &str, fields: &str, tmeta: bool, vmeta: bool, v: impl Trait) {
    if !(v.shape_top() == top && v.shape_fields() == fields && v.has_tmeta() == tmeta
        && v.has_vmeta() == vmeta)
    {
        { ::std::rt::begin_panic("explicit panic") }
    }
}
fn test_get_usize(some_usize: Option<usize>, v: impl GetUsize) {
    if !(v.get_usize() == some_usize) {
        { ::std::rt::begin_panic("explicit panic") }
    }
}
fn main() {
    static_test();
    test("struct", "unit", false, false, Unit);
    test("struct", "tuple", true, false, Tuple(0));
    test("struct", "named", false, true, Struct { field: 0 });
    test("enum", "unit", false, true, Enum::Unit);
    test("enum", "tuple", false, true, Enum::Tuple(0));
    test("enum", "named", false, false, Enum::Named { field: 0 });
    test("union", "named", false, false, Union { field: 0 });
    test_get_usize(None, Unit);
    test_get_usize(Some(0), Tuple(0));
    test_get_usize(Some(0), Struct { field: 0 });
    test_get_usize(None, Enum::Unit);
    test_get_usize(Some(0), Enum::Tuple(0));
    test_get_usize(None, Enum::Named { field: 0u32 });
}
