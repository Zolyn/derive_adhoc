//! Test/example for const generics, including default values
//!
//! This is a separate file rather than (say) integrated into
//! `partial-ord.rs` or `ref-version.rs` because this feature is new
//! in Rust and our MSRV can't compile it.
//!
//! `recent` in the filename arranges for it not to be run by the MSRV
//! compiler.

#![allow(dead_code)]

use derive_adhoc::{derive_adhoc, Adhoc};

#[derive(Adhoc)]
enum Enum<T = (), const N: usize = 1> {
    Unit,
    Tuple([T; N]),
}

derive_adhoc! {
    Enum:

    $tvis $tdefkwd ${paste $tdeftype Copy}
    ${tdefvariants $(
        ${vdefbody $vname $(
            $fvis ${fdefine $fname} $ftype,
        ) }
    ) }

    impl<$tgens> From<$ttype> for ${paste $ttype Copy} {
        fn from(orig: $ttype) -> Self {
            match orig { $(
                ${vpat} => ${vtype self=${paste $ttype Copy}} { $(
                    $fname: $fpatname,
                ) },
            ) }
        }
    }
}

fn main() {
    let _: Option<Enum> = None;
    let _ = Enum::Unit::<u8, 3>;
    let _ = Enum::Tuple([42; 2]);
}
