//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT

// Example which derives PartialOrd, treating different enum variants
// as incomparable.
//
// This gives a demonstration on how to handle two enum
// values (from the same enum) at once - in particular, patterns
// with a different prefix.

use std::cmp::Ordering::{self, *};

use derive_adhoc::{define_derive_adhoc, Adhoc};

define_derive_adhoc! {
    VeryPartialOrd =

    // ${vpat    fprefix=f_ self=$tname vname=$vname}
    // ${vconstr fprefix=f_ self=$ttype vname=$vname}
    //    each is a single template element, or in {...}
    //    defaults shown
    //    vname not expanded in structs
    // expands to something like
    //    SELF ${if is_enum {:: VNAME}} { $(
    //        $fname: ${paste FPREFIX $fname}
    //    ) }
    impl<$tgens> PartialOrd for $ttype
    where $( $ftype: PartialOrd, )
          $twheres
    {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            match (self, other) {
              $(
                (${vpat fprefix=self_}, ${vpat fprefix=other_}) => {
                    let ord = Equal;
                  $(
                    let ord = ord.then(PartialOrd::partial_cmp(
                        ${paste self_ $fname}, ${paste other_ $fname},
                    )?);
                  )
                Some(ord) // (misindented by rustfmt)
                },
              )
                _ => None,
            }
        }
    }
}

#[derive(Adhoc, PartialEq)]
#[derive_adhoc(VeryPartialOrd)]
enum Enum<F: PartialEq, G>
where
    G: PartialEq,
{
    Unit,
    Tuple(F),
    Struct { field: G },
}

fn mk_t_struct<F: PartialEq>(field: &str) -> Enum<F, &str> {
    Enum::Struct { field }
}

fn main() {
    // TODO use default type parameters to get rid of all these turbofish
    // (but currently, that breaks)
    use Enum::*;
    expect_none(Unit::<_, ()>, Tuple(42));
    expect_none(Tuple(42), mk_t_struct(""));
    expect_none(Tuple::<_, ()>(Tuple::<_, ()>(42)), Tuple(Unit));

    expect_some(Unit::<(), ()>, Unit::<(), ()>, Equal);
    expect_some(Tuple::<_, ()>(0), Tuple(0), Equal);
    expect_some(Tuple::<_, ()>(1), Tuple(2), Less);
    expect_some(Tuple::<_, ()>(4), Tuple(3), Greater);
    expect_some(mk_t_struct::<()>("a"), mk_t_struct("a"), Equal);
    expect_some(mk_t_struct::<()>("b"), mk_t_struct("c"), Less);
    expect_some(mk_t_struct::<()>("e"), mk_t_struct("d"), Greater);
}

/// Versions of assert, basically
///
/// Without too many macros cluttering the expanded output
fn expect(ok: bool) {
    if !ok {
        panic!();
    }
}
fn expect_none<T: PartialOrd>(a: T, b: T) {
    expect(a.partial_cmp(&b) == None);
}
fn expect_some<T: PartialOrd>(a: T, b: T, ord: Ordering) {
    expect(a.partial_cmp(&b) == Some(ord));
}
