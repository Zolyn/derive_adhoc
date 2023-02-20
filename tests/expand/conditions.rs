//! Test cases for conditions
//!
//! This gives a basic demonstration of how to handle an enum.

use derive_adhoc::{define_derive_adhoc, derive_adhoc, Adhoc};

trait Trait {
    fn shape_top(&self) -> &'static str;
    fn shape_fields(&self) -> &'static str;
    fn has_vmeta(&self) -> bool;
}

define_derive_adhoc! {
    Trait =

    impl Trait for $ttype {
        fn shape_top(&self) -> &'static str {
            ${select1
              is_struct { "struct" }
              is_enum { "enum" }
              is_union { "union" }
            }
        }
        fn shape_fields(&self) -> &'static str {
            #[allow(unused_unsafe)]
            unsafe {
                match self { $(
                    #[allow(unused_variables)]
                    $vpat => ${select1
                               v_is_unit { "unit" }
                               v_is_tuple { "tuple" }
                               v_is_named { "named" }
                    },
                ) }
            }
        }
        fn has_vmeta(&self) -> bool {
            #[allow(unused_unsafe)]
            unsafe {
                match self { $(
                    #[allow(unused_variables)]
                    // TODO maybe ${bool COND} for ${if ... true ... false} ?
                    $vpat => ${if vmeta(hello(there))
                               { true }
                               else { false }},
                ) }
            }
        }
    }
}

#[derive(Adhoc)]
#[derive_adhoc(Trait)]
struct Unit;

#[derive(Adhoc)]
#[derive_adhoc(Trait)]
struct Tuple(usize);

#[derive(Adhoc)]
#[derive_adhoc(Trait)]
struct Struct {
    field: usize,
}

#[derive(Adhoc)]
#[derive_adhoc(Trait)]
enum Enum {
    Unit,
    Tuple(usize),
    Named { field: usize },
}

#[derive(Adhoc)]
#[derive_adhoc(Trait)]
union Union {
    field: usize,
}

derive_adhoc! {
    Unit:

    fn static_test() {
        // bad is an error
        ${if false             { bad }}
        ${if true              {} else { bad }}
        ${if not(false)        {} else { bad }}
        ${if not(true)         { bad }}
        ${if any()             { bad }}
        ${if any(false)        { bad }}
        ${if any(true)         {} else { bad }}
        ${if any(true,false)   {} else { bad }}
        ${if any(false,true)   {} else { bad }}
        ${if all()             {} else { bad }}
        ${if all(false)        { bad }}
        ${if all(true)         {} else { bad }}
        ${if all(true,false)   { bad }}
        ${if all(false,true)   { bad }}
    }
}

fn test(top: &str, fields: &str, vmeta: bool, v: impl Trait) {
    if !(v.shape_top() == top
        && v.shape_fields() == fields
        && v.has_vmeta() == vmeta)
    {
        panic!()
    }
}

fn main() {
    static_test();

    test("struct", "unit", false, Unit);
    test("struct", "tuple", false, Tuple(0));
    test("struct", "named", false, Struct { field: 0 });
    test("enum", "unit", false, Enum::Unit);
    test("enum", "tuple", false, Enum::Tuple(0));
    test("enum", "named", false, Enum::Named { field: 0 });
    test("union", "named", false, Union { field: 0 });
}
