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
        fn has_tmeta(&self) -> bool {
            ${if tmeta(hi(ferris))
              { true }
              else { false }}
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

trait GetUsize {
    fn get_usize(&self) -> Option<usize>;
}

define_derive_adhoc! {
    GetUsize =

    impl GetUsize for $ttype {
        fn get_usize(&self) -> Option<usize> {
            match self { $(
                #[allow(unused_variables)]
                $vpat => { $(
                    ${when approx_equal($ftype, usize)}
                    return Some(*$fpatname);
                ) }
            ) };
            #[allow(unreachable_code)]
            None
        }
    }
}

#[derive(Adhoc)]
#[derive_adhoc(Trait, GetUsize)]
struct Unit;

#[derive(Adhoc)]
#[derive_adhoc(Trait, GetUsize)]
#[adhoc(hi(ferris))]
struct Tuple(usize);

#[derive(Adhoc)]
#[derive_adhoc(Trait, GetUsize)]
#[adhoc(hello(there = 42))]
struct Struct {
    field: usize,
}

#[derive(Adhoc)]
#[derive_adhoc(Trait, GetUsize)]
enum Enum {
    #[adhoc(hello(there))]
    Unit,
    #[adhoc(hello(there(inner)))]
    Tuple(usize),
    Named {
        field: u32,
    },
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

fn test(top: &str, fields: &str, tmeta: bool, vmeta: bool, v: impl Trait) {
    if !(v.shape_top() == top
        && v.shape_fields() == fields
        && v.has_tmeta() == tmeta
        && v.has_vmeta() == vmeta)
    {
        panic!()
    }
}

fn test_get_usize(some_usize: Option<usize>, v: impl GetUsize) {
    if !(v.get_usize() == some_usize) {
        panic!()
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
