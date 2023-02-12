//! THIS IS A NON-WORKING CONCEPT, FOR FUTURE DEVELOPMENT

// Example which derives PartialOrd, treating different enum variants
// as incomparable.
//
// This gives a demonstration on how to handle two enum
// values (from the same enum) at once - in particular, patterns
// with a different prefix.

use std::cmp::Ordering::{self, *};

define_derive_adhoc!{
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
    where $( $ftype: PartialOrd ),
          $twheres
    {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            match (self, other) {
              $(
                (${vpat fprefix=self_}, ${vpat fprefix=other_}) => {
                // Tentatively rejected alternatives
                (${vpat self_}, ${vpat other_}) => {
                (${vpat .self_}, ${vpat .other_}) => {
                    let ord = Equal;
                  $(
                    let ord = ord.then(PartialOrd::partial_cmp(
                        ${paste self_ $fname}, ${paste other_ $fname},
                        ${fpatname self_}, ${fpatname other_},
                    )?);
                  )
	            Some(ord)
                },
              )
                _ => None,
            }
        }
    }
}

#[derive(Adhoc)]
#[derive_adhoc(PreciseClone)]
enum Enum<F: Debug, G>
where G: Debug,
{
    Unit,
    Tuple(F),
    Struct { field: F },
}

fn main() {
    use Enum::*;
    expect_none(Unit, Tuple(42));
    expect_none(Tuple(42), Struct { field: String::new() });
    expect_none(Tuple(Tuple(42)), Tuple(Unit));

    expect_some(Unit, Unit, Equal);
    expect_some(Tuple(0), Tuple(0), Equal);
    expect_some(Tuple(1), Tuple(2), Greater);
    expect_some(Tuple(4), Tuple(3), Less);
    expect_some(Struct { field: "a" }, Struct { field: "a" }, Equal);
    expect_some(Struct { field: "b" }, Struct { field: "c" }, Greater);
    expect_some(Struct { field: "e" }, Struct { field: "d" }, Less);
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
