//! Example which derives a new type containing references
//!
//! This demonstrates how to make a new type which mirrors the driver type,
//! including both structs and enums, with units, tuples or structs.
//!
//! It also demonstrates how to construct a new enum type
//! using `$vdefvariants`, and `$vdefbody` and `$fdefine}`,
//! and handling of visibility attributes.

#![allow(dead_code)]

use derive_adhoc::{define_derive_adhoc, Adhoc};

define_derive_adhoc! {
    ReferenceVersion =

    $tvis $tdefkwd ${paste $tname Reference}<'reference, $tdefgens>
    ${tdefvariants $(
        ${vdefbody $vname $(
            $fvis ${fdefine $fname} &'reference $ftype,
        ) }
    ) }

    impl<'reference, $tgens> From<&'reference $ttype>
    for ${paste $tname Reference}<'reference, $tgnames> {
        fn from(ref_to_owned: &'reference $ttype) -> Self {
            match ref_to_owned { $(
                $vpat => ${vtype self=${paste $ttype Reference}} { $(
                    $fname: $fpatname,
                ) },
            ) }
        }
    }

    impl<'reference, $tgens> ${paste $tname Reference}<'reference, $tgnames>
    where $( $ftype: Clone, )
    {
        fn cloned(&self) -> $ttype {
            match self { $(
                ${vpat self=Self} => $vtype { $(
                    $fname: (**$fpatname).clone(),
                ) },
            ) }
        }
    }
}

//   We can't do this for a Unit because it would end up with
//   an unused lifetime.
// #[derive(Adhoc)]
// #[derive_adhoc(ReferenceVersion)]
// struct Unit;

#[derive(Adhoc)]
#[derive_adhoc(ReferenceVersion)]
struct Tuple<F = ()>(F);

#[derive(Adhoc)]
#[derive_adhoc(ReferenceVersion)]
struct Struct<F = ()> {
    field: F,
}

#[derive(Adhoc)]
#[derive_adhoc(ReferenceVersion)]
enum Enum<F = ()> {
    Unit,
    Tuple(F),
    Struct { field: F },
}

fn main() {
    let _: Option<EnumReference> = None;
    let _: Option<EnumReference::<i32>> = None;
}
