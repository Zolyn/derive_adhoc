//! Test `${define }` and `${defcond }`
#![allow(dead_code)]
use derive_adhoc::{derive_adhoc, Adhoc};
struct S {
    a: i32,
    b: i32,
}
const X: i32 = 0;
const L: &[(i32, i32)] = &[
    (901, 901),
    (141, 141),
    (902, 902),
    (102, 102),
    (902, 902),
    (202, 202),
    (903, 903),
    (203, 203),
    (343, 343),
    (902, 902),
    (202, 202),
    (903, 903),
    (203, 203),
    (343, 343),
];
fn main() {
    for (exp, got) in L {
        if exp != got {
            std::process::abort()
        }
    }
}
